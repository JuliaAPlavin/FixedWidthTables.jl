import FixedWidthTables as FWT
using Test


import Aqua
import CompatHelperLocal as CHL
@testset begin
    CHL.@check()
    Aqua.test_ambiguities(FWT, recursive=false)
    Aqua.test_unbound_args(FWT)
    Aqua.test_undefined_exports(FWT)
    Aqua.test_stale_deps(FWT)
end

@testset begin
    a = FWT.read("rfc_2020b_cat.txt",
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

    @test_throws ArgumentError FWT.read("rfc_2020b_cat.txt",
        (
            cat = (1:1, Char),
            name = (4:11, String),
            cnt = (80:85, Int),
        ),
    )

    b = FWT.read("0005m23.umrao", skiprows=1:1, headerrow=2)

    # c = Fi
end
