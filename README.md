# FixedWidthTables.jl

Read fixed width (so-called ASCII) tables. A wide range of format specification options, including autodetection.

Basic usage:
```julia
julia> using FixedWidthTables

julia> io = IOBuffer("""
       123456
       789101
       """)

julia> FixedWidthTables.read(io, (
           x=(1:2, Int),
           y=(3:5, Float64),
           z=(6:6, Char),
       ))
2-element StructArray(::Vector{Int64}, ::Vector{Float64}, ::Vector{Char}) with eltype NamedTuple{(:x, :y, :z), Tuple{Int64, Float64, Char}}:
 (x = 12, y = 345.0, z = '6')
 (x = 78, y = 910.0, z = '1')
```

See source code and tests for more examples and details.
