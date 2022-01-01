using FixedWidthTables
using Test


import Aqua
import CompatHelperLocal
@testset begin
    CompatHelperLocal.@check()
    Aqua.test_ambiguities(FixedWidthTables, recursive=false)
    Aqua.test_unbound_args(FixedWidthTables)
    Aqua.test_undefined_exports(FixedWidthTables)
    Aqua.test_stale_deps(FixedWidthTables)
end

@testset begin
    a = FixedWidthTables.read("rfc_2020b_cat.txt",
        (
            cat = (1:1, Char),
            name = (4:11, String),
            cnt = (80:85, Int),
        ),
        allow_shorter_lines=true, skiprows_startwith=["#"],
    )
    @test length(a) == 73
    @test length(unique(a)) == 73
    @test a[1] == (cat = 'N', name = "2357-141", cnt = 17)

    @test_throws ArgumentError FixedWidthTables.read("rfc_2020b_cat.txt",
        (
            cat = (1:1, Char),
            name = (4:11, String),
            cnt = (80:85, Int),
        ),
    )
end
