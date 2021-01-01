using FixedWidthTables
using Test


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
