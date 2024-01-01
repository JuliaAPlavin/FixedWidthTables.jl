module FixedWidthTables

using DataPipes
using StructArrays
using AccessorsExtra  # for Dictionaries support


convert_val(::Type{AbstractString},     val::AbstractString) = val  # e.g. SubString in case it is needed as-is
convert_val(T::Type{<:AbstractString},  val::AbstractString) = T(val)  # e.g. InlineString
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


"""
    read(io, [sink=StructArray], colspecs::Union{NamedTuple, Dictionary}; skiprows=Int[], skiprows_startwith=String[], missings=String[], strip_chars=[' '], allow_shorter_lines=false, allow_overlap=false, restrict_remaining_chars=nothing)
    read(io, [sink=StructArray]; headerrow::Union{Int, AbstractString}, skiprows=Int[], skiprows_startwith=String[], missings=String[], delim=' ')
"""
function read end

read(io, colspecs; kwargs...) = read(io, StructArray, colspecs; kwargs...)
read(io; kwargs...) = read(io, StructArray; kwargs...)

function read(io, sink::Type;
        skiprows=Int[], skiprows_startwith=String[], missings=String[],
        delim=' ',
        headerrow::Union{Int, AbstractString})
    frs = FilterRowsSpec(skip_indices=skiprows, pred=line -> !any(startswith.(line, skiprows_startwith)))
    restrictions = Restrictions(;
        allow_shorter_lines=true,
        restrict_unused_chars=c -> test_match(delim, c),
    )

    ixlines = ixlineiterator(io, frs) |> collect

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
    colspecs = ColSpecs(_colspecs_from_pairs(sink, colspecs); allow_overlap=false)

    filter!(((i,l),) -> i != headerrow, ixlines)
    cols = map(T -> T[], spec_types(colspecs))
    cols = materialize!(cols, Iterators.Stateful(ixlines); colspecs, restrictions, strip_chars=delim, missings)
    return sink(cols)
end

function read(io, sink, colspecs;
        skiprows=Int[], skiprows_startwith=String[], missings=String[], strip_chars=[' '],
        allow_shorter_lines=false, allow_overlap=false, restrict_remaining_chars=nothing)
    frs = FilterRowsSpec(skip_indices=skiprows, pred=line -> !any(sw -> startswith(line, sw), skiprows_startwith))
    colspecs = ColSpecs(colspecs; allow_overlap)
    restrictions = Restrictions(;
        allow_shorter_lines,
        restrict_unused_chars=isnothing(restrict_remaining_chars) ? Returns(true) : ∈(restrict_remaining_chars),
    )
    cols = map(T -> T[], spec_types(colspecs))
    cols = materialize!(cols, ixlineiterator(io, frs); colspecs, restrictions, strip_chars, missings)
    return sink(cols)
end

function materialize!(cols, ixlines; colspecs, restrictions, kwargs...)
    for (i, line) in ixlines
        newcols = try
            _push_row_from_line!(cols, line, colspecs, restrictions; kwargs...)
        catch e
            rethrow(ErrorException("Error on line $i: '$line' \n$e"))
        end
        if newcols !== cols
            return materialize!(newcols, ixlines; colspecs, restrictions, kwargs...)
        end
    end
    return cols
end


_colspecs_from_pairs(::Type{StructArray}, pairs) = (; pairs...)

Base.@kwdef struct FilterRowsSpec{TI <: AbstractVector{Int}, TC}
    skip_indices::TI
    pred::TC
end

ixlineiterator(io, frs::FilterRowsSpec) =
    Iterators.filter(enumerate(eachline(io))) do (i, line)
        i ∉ frs.skip_indices && frs.pred(line)
    end

Base.@kwdef struct ColSpecs{TR, TT}
    char_rngs::TR
    types::TT
    used_chars::Vector{Bool}
end

spec_types(cs::ColSpecs) = cs.types
spec_types(cs::Vector{<:ColSpecs}) = spec_types(first(cs))

max_used_index(cs::ColSpecs) = length(cs.used_chars)

function ColSpecs(specs; allow_overlap::Bool)
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

ColSpecs(specs::AbstractVector; allow_overlap::Bool) = ColSpecs.(specs; allow_overlap)


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


function _push_row_from_line!(cols, line::AbstractString, cs::ColSpecs, rs::Restrictions; kwargs...)
    cr = check_restrictions(line, rs, cs)
    if !isnothing(cr)
        throw(ArgumentError(cr))
    end

    parse_row!(cols, line, cs; kwargs...)
end

function _push_row_from_line!(cols, line::AbstractString, cses::Vector{<:ColSpecs}, rs::Restrictions; kwargs...)
    for (j, cs) in cses |> enumerate
        cr = check_restrictions(line, rs, cs)
        if !isnothing(cr)
            j == lastindex(cses) ?
                throw(ArgumentError(cr)) :
                continue
        end

        return parse_row!(cols, line, cs; kwargs...)
    end
end


function parse_row!(cols, line::AbstractString, cs::ColSpecs; strip_chars, missings, lastk=nothing)
    skipped = lastk === nothing
    for (k, col, rng, typ) in zip(keys(cols), cols, cs.char_rngs, cs.types)
        if !skipped
            if k == lastk
                skipped = true
            end
            continue
        end
        s_val_ = @view line[rng.start:min(end, rng.stop)]
        s_val = strip(s_val_, strip_chars)
        is_miss = any(ms -> test_match(ms, s_val), missings)
        f_val = is_miss ? missing : convert_val(typ, s_val)
        newcol = _push!!(col, f_val)
        if newcol !== col
            return parse_row!((@set cols[k] = newcol), line, cs; strip_chars, missings, lastk=k)
        end
    end
    return cols
end


_push!!(col::AbstractVector{T}, val::T) where {T} = push!(col, val)
_push!!(col::AbstractVector{T}, val::Union{T, Missing}) where {T} = push!(convert(AbstractVector{Union{T,Missing}}, col), val)

end
