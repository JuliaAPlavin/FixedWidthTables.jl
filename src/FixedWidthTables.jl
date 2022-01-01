module FixedWidthTables

using IterTools

convert_val(::Type{<:AbstractString},   val::AbstractString) = val  # e.g. SubString in case it is needed as-is
convert_val(::Type{String},             val::AbstractString) = string(val)
convert_val(::Type{Char},               val::AbstractString) = only(val)
convert_val(::Type{Symbol},             val::AbstractString) = Symbol(val)
convert_val(typ::Type{<:Integer},       val::AbstractString) = parse(typ, val)
convert_val(typ::Type{<:AbstractFloat}, val::AbstractString) = parse(typ, replace(val, 'D' => 'E'))

test_match(target::String,   s::AbstractString) = s == target
test_match(target::Regex,    s::AbstractString) = occursin(target, s)
test_match(target::Function, s::AbstractString) = target(s)


function read(io;
        skiprows=[], skiprows_startwith=[], missings=[],
        delim=' ',
        headerrow=nothing)
    lines = readlines(io)
    ixlines = filter(((i, line),) -> i ∉ skiprows && !any(startswith.(line, skiprows_startwith)), enumerate(lines) |> collect)
    header = only(line for (i, line) in ixlines if i == headerrow)

    maxlen = maximum(il -> length(il[2]), ixlines)
    char_flags = fill(:delim, maxlen)
    for (i, line) in ixlines
        char_flags[findall((!=)(delim), collect(line))] .= :value
    end
    for (i, ch) in enumerate(collect(header))
        if ch == delim && char_flags[i] != :delim
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
        strip(header[min.(rng, length(header))], delim)
        for rng in field_ranges
    ]

    colspecs = map(field_names, field_ranges) do name, rng
        Symbol(name) => (rng, String)
    end
    colspecs = (; colspecs...)

    ixlines = filter!(((i,l),) -> i != headerrow, ixlines)
    map(ixlines) do (i, line)
        try
            map(colspecs) do (rng, typ)
                @assert step(rng) == 1
                s_val = line[rng.start:min(length(line), rng.stop)]
                s_val = strip(s_val, delim)
                is_miss = any(ms -> test_match(ms, s_val), missings)
                f_val = is_miss ? missing : convert_val(typ, s_val)
            end
        catch e
            rethrow(ErrorException("$e \n on line $i: '$line'"))
        end
    end
end

function read(io, colspecs::NamedTuple;
        skiprows=[], skiprows_startwith=[], missings=[], strip_chars=[' '],
        allow_shorter_lines=false, allow_overlap=false, restrict_remaining_chars=nothing)
    lines = eachline(io)
    ixlines = filter(((i, line),) -> i ∉ skiprows && !any(startswith.(line, skiprows_startwith)), enumerate(lines) |> collect)

    max_used_index = maximum(((rng, typ),) -> maximum(rng), colspecs)
    used_chars = zeros(Bool, max_used_index)
    for (name, (rng, typ)) in colspecs |> pairs
        if !allow_overlap
            if any(used_chars[rng])
                ArgumentError("column $name overlaps with another") |> throw
            end
        end
        used_chars[rng] .= true
    end

    map(ixlines) do (i, line)
        if restrict_remaining_chars != nothing
            unused_portion = if allow_shorter_lines
                line[filter(ix -> ix <= length(line), findall(.!used_chars))]
            else
                line[findall(.!used_chars)]
            end
            for block in [
                    line[max_used_index + 1:end],
                    unused_portion,
                ]
                if !isempty(setdiff(block, restrict_remaining_chars))
                    ArgumentError("disallowed characters $(setdiff(block, restrict_remaining_chars)) in line $i: '$line'") |> throw
                end
            end
        end
        if !allow_shorter_lines && length(line) < max_used_index
            ArgumentError("line $i length is $(length(line)), shorter than expected $max_used_index (use allow_shorter_lines=true if indended): '$line'") |> throw
        end

        try
            map(colspecs) do (rng, typ)
                @assert step(rng) == 1
                s_val = if allow_shorter_lines
                    line[rng.start:min(length(line), rng.stop)]
                else
                    line[rng]
                end
                s_val = strip(s_val, strip_chars)
                is_miss = any(ms -> test_match(ms, s_val), missings)
                f_val = is_miss ? missing : convert_val(typ, s_val)
            end
        catch e
            rethrow(ErrorException("$e \n on line $i: '$line'"))
        end
    end
end

end
