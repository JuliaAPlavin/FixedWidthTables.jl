import FixedWidthTables as FWT
using Test


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
    @test b[1] == (Date = "810813", var"Freq." = "8.0", var"U.T." = "8.005", S = "0.37", SigS = "0.05", P = "", SigP = "", Xi = "", SigXi = "", N = "1")

    c = FWT.read("S_0048-097.txt", skiprows=1:1, headerrow=2)
    @test keys(c[1]) == (:YYYYMMDD, Symbol("Freq."), Symbol("U.T."), :S, :SigS, :N, :_field_7, :_field_8, :_field_9)

    d = FWT.read("rfc_2021a_z.txt", headerrow="J2000      B1950    | UNK| fullname                      |ratext      |dectext     |class |  redshift|flag|redshift_ref       ", delim=[' ', '|'])
    @test d[1] == (J2000 = "J0000+0307", B1950 = "2357+028", UNK = "0.1", fullname = "SDSS J000027.01+030715.5", ratext = "00h00m27.02s", dectext = "+03d07m15.6s", class = "RadioS", redshift = "2.353124", flag = "SPEC", redshift_ref = "2016SDSSD.C...0000:")
end


import Aqua
import CompatHelperLocal as CHL
@testset begin
    CHL.@check()
    Aqua.test_ambiguities(FWT, recursive=false)
    Aqua.test_unbound_args(FWT)
    Aqua.test_undefined_exports(FWT)
    Aqua.test_stale_deps(FWT)
end
