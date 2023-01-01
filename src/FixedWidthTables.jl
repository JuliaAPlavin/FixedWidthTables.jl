module FixedWidthTables

using StructArrays


convert_val(::Type{<:AbstractString},   val::AbstractString) = val  # e.g. SubString in case it is needed as-is
convert_val(::Type{String},             val::AbstractString) = string(val)
convert_val(::Type{Char},               val::AbstractString) = only(val)
convert_val(::Type{Symbol},             val::AbstractString) = Symbol(val)
convert_val(typ::Type{<:Integer},       val::AbstractString) = parse(typ, val)
convert_val(typ::Type{<:AbstractFloat}, val::AbstractString) = parse(typ, replace(val, 'D' => 'E'))

test_match(target::String,   s::AbstractString) = s == target
test_match(target::Regex,    s::AbstractString) = occursin(target, s)
test_match(target::Function, s::AbstractString) = target(s)
test_match(target::Char,   s::Char) = s == target
test_match(target::Vector{Char},   s::Char) = s ∈ target


function read(io;
        skiprows=Int[], skiprows_startwith=String[], missings=String[],
        delim=' ',
        headerrow::Union{Int, AbstractString})
    frs = FilterRowsSpec(skip_indices=skiprows, pred=line -> !any(startswith.(line, skiprows_startwith)))
    restrictions = Restrictions(;
        allow_shorter_lines=true,
        restrict_unused_chars=c -> test_match(delim, c),
    )

    ixlines = process(frs, eachline(io))

    header = isa(headerrow, AbstractString) ? headerrow : only(line for (i, line) in ixlines if i == headerrow)

    maxlen = maximum(il -> length(il[2]), ixlines)
    char_flags = fill(:delim, maxlen)
    for (i, line) in ixlines
        char_flags[findall(c -> !test_match(delim, c), collect(line))] .= :value
    end
    for (i, ch) in enumerate(collect(header))
        if test_match(delim, ch) && char_flags[i] != :delim
            char_flags[i] = :value_but_header_delim
        end
    end

    field_ranges = let
        rngs1 = [(flag=:delim, rng=0:0)]
        for (i, flag) in enumerate(char_flags)
            if rngs1[end].flag == flag
                rngs1[end] = (; rngs1[end].flag, rng=rngs1[end].rng[1]:i)
            elseif (rngs1[end].flag => flag) == (:value_but_header_delim => :value)
                rngs1[end] = (; flag=:value, rng=rngs1[end].rng[1]:i)
            else
                push!(rngs1, (; flag, rng=i:i))
            end
        end
        filter!(fr -> fr.flag != :delim, rngs1)

        rngs2 = [popfirst!(rngs1)]
        for rng in rngs1
            if (rngs2[end].flag => rng.flag) == (:value => :value_but_header_delim)
            rngs2[end] = (; flag=:value, rng=rngs2[end].rng[1]:rng.rng[end])
            else
                push!(rngs2, rng)
            end
        end
        rngs2

        map(r -> r.rng, rngs2)
    end
    
    field_names = [
        isdisjoint(rng, eachindex(header)) ? "_field_$i" : strip(header[intersect(rng, eachindex(header))], delim)
        for (i, rng) in enumerate(field_ranges)
    ]

    colspecs = map(field_names, field_ranges) do name, rng
        Symbol(name) => (rng, String)
    end
    colspecs = ColSpecs((; colspecs...); allow_overlap=false)

    ixlines = filter!(((i,l),) -> i != headerrow, ixlines)

    res = StructArray(map(T -> T[], spec_types(colspecs)))
    for (i, line) in ixlines
        cr = check_restrictions(line, restrictions, colspecs)
        isnothing(cr) || throw(ArgumentError("Line $i: " * cr))

        try
            r = parse_row(line, colspecs; strip_chars=delim, missings)
            res = StructArrays.append!!(res, (r,))
        catch e
            rethrow(ErrorException("$e \n on line $i: '$line'"))
        end
    end
    res
end


function read(io, colspecs;
        skiprows=Int[], skiprows_startwith=String[], missings=String[], strip_chars=[' '],
        allow_shorter_lines=false, allow_overlap=false, restrict_remaining_chars=nothing)
    frs = FilterRowsSpec(skip_indices=skiprows, pred=line -> !any(startswith.(line, skiprows_startwith)))
    colspecs = ColSpecs(colspecs; allow_overlap)
    restrictions = Restrictions(;
        allow_shorter_lines,
        restrict_unused_chars=isnothing(restrict_remaining_chars) ? Returns(true) : ∈(restrict_remaining_chars),
    )

    ixlines = process(frs, eachline(io))

    res = StructArray(map(T -> T[], spec_types(colspecs)))
    for (i, line) in ixlines
        r = process_line(i, line, colspecs, restrictions; strip_chars, missings)
        res = StructArrays.append!!(res, (r,))
    end
    res
end


Base.@kwdef struct FilterRowsSpec{TI <: AbstractVector{Int}, TC}
    skip_indices::TI
    pred::TC
end

process(frs::FilterRowsSpec, lines) =
    filter(enumerate(lines) |> collect) do (i, line)
        i ∉ frs.skip_indices && frs.pred(line)
    end

Base.@kwdef struct ColSpecs{TR <: NamedTuple, TT <: NamedTuple}
    char_rngs::TR
    types::TT
    used_chars::Vector{Bool}
end

spec_types(cs::ColSpecs) = cs.types
spec_types(cs::Vector{<:ColSpecs}) = spec_types(first(cs))

max_used_index(cs::ColSpecs) = length(cs.used_chars)

function ColSpecs(specs::NamedTuple; allow_overlap::Bool)
    @assert eltype(specs) <: Tuple{UnitRange{Int}, DataType}
    char_rngs = map(first, specs)
    types = map(last, specs)
    max_used_index = maximum(maximum, char_rngs)
    used_chars = zeros(Bool, max_used_index)
    for (name, rng) in char_rngs |> pairs
        if !allow_overlap
            if any(used_chars[rng])
                ArgumentError("column $name overlaps with another") |> throw
            end
        end
        used_chars[rng] .= true
    end
    return ColSpecs(; char_rngs, types, used_chars)
end

ColSpecs(specs::Vector{<:NamedTuple}; allow_overlap::Bool) = ColSpecs.(specs; allow_overlap)

function parse_row(line::AbstractString, cs::ColSpecs; strip_chars, missings)
    map(cs.char_rngs, cs.types) do rng, typ
        @assert step(rng) == 1
        s_val = line[rng.start:min(end, rng.stop)]
        s_val = strip(s_val, strip_chars)
        is_miss = any(ms -> test_match(ms, s_val), missings)
        f_val = is_miss ? missing : convert_val(typ, s_val)
    end
end


Base.@kwdef struct Restrictions{TP}
    allow_shorter_lines::Bool
    restrict_unused_chars::TP
end

function check_restrictions(line::AbstractString, rs::Restrictions, cs::ColSpecs)
    if !rs.allow_shorter_lines && length(line) < max_used_index(cs)
        return "line length is $(length(line)), shorter than expected $max_used_index (use allow_shorter_lines=true if indended): '$line'"
    end
    if rs.restrict_unused_chars !== Returns(true)
        unused = @views line[findall(!, cs.used_chars[1:min(end, length(line))])] * line[max_used_index(cs) + 1:end]
        disallowed_chars = filter(!rs.restrict_unused_chars, unused)
        isempty(disallowed_chars) || return "disallowed characters $(disallowed_chars) in '$line'"
    end
    return nothing
end


function process_line(i::Int, line::AbstractString, cs::ColSpecs, rs::Restrictions; strip_chars, missings)
    cr = check_restrictions(line, rs, cs)
    if !isnothing(cr)
        throw(ArgumentError("Line $i: " * cr))
    end

    try
        return parse_row(line, cs; strip_chars, missings)
    catch e
        rethrow(ErrorException("$e \n on line $i: '$line'"))
    end
end

function process_line(i::Int, line::AbstractString, cses::Vector{<:ColSpecs}, rs::Restrictions; strip_chars, missings)
    for (j, cs) in cses |> enumerate
        cr = check_restrictions(line, rs, cs)
        if !isnothing(cr)
            j == lastindex(cses) ?
                throw(ArgumentError("Line $i: " * cr)) :
                continue
        end

        try
            return parse_row(line, cs; strip_chars, missings)
        catch e
            rethrow(ErrorException("$e \n on line $i: '$line'"))
        end
    end
end

end
