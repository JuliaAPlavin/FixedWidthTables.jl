import FixedWidthTables as FWT
using Test


@testset begin
    a = FWT.read("rfc_2020b_cat.txt",
        (
            cat = (1:1, Char),
            name = (4:11, String),
            cnt = (80:85, Int),
            s_flux = (88:93, Float64),
        ),
        allow_shorter_lines=true, skiprows_startwith=["#"], missings=["-1.00"],
    )
    @test length(a) == 73
    @test length(unique(a)) == 73
    @test isequal(a[1], (cat = 'N', name = "2357-141", cnt = 17, s_flux=missing))
    @test isequal(a.s_flux, [missing, missing, missing, missing, missing, 0.569, 0.115, missing, 0.035, missing, 0.842, 0.327, missing, 0.149, missing, 0.319, missing, missing, missing, 0.204, missing, missing, missing, 0.018, missing, missing, missing, 0.009, missing, missing, missing, missing, missing, missing, missing, 0.056, 0.006, missing, missing, missing, missing, missing, missing, missing, missing, missing, missing, missing, missing, missing, 0.227, 0.183, 0.164, missing, 0.203, missing, missing, 0.07, missing, missing, missing, 0.297, missing, missing, missing, missing, missing, missing, 0.633, missing, missing, missing, missing])
    @test eltype(a) == @NamedTuple{cat::Char, name::String, cnt::Int, s_flux::Union{Float64, Missing}}

    @test_throws ArgumentError FWT.read("rfc_2020b_cat.txt",
        (
            cat = (1:1, Char),
            name = (4:11, String),
            cnt = (80:85, Int),
        ),
    )

    b = FWT.read("0005m23.umrao", skiprows=1:1, headerrow=2)
    @test b[1] == (Date = "810813", var"Freq." = "8.0", var"U.T." = "8.005", S = "0.37", SigS = "0.05", P = "", SigP = "", Xi = "", SigXi = "", N = "1")
    @test b.Date == ["810813", "850422", "860717", "860801", "860815"]
    @test isconcretetype(eltype(b))

    c = FWT.read("S_0048-097.txt", skiprows=1:1, headerrow=2)
    @test keys(c[1]) == (:YYYYMMDD, Symbol("Freq."), Symbol("U.T."), :S, :SigS, :N, :_field_7, :_field_8, :_field_9)
    @test c.S == ["1.62", "0.10"]
    @test isconcretetype(eltype(c))

    d = FWT.read("rfc_2021a_z.txt",
        headerrow="J2000      B1950    | UNK| fullname                      |ratext      |dectext     |class |  redshift|flag|redshift_ref       ",
        delim=[' ', '|'], missings=[""])
    @test d[1] == (J2000 = "J0000+0307", B1950 = "2357+028", UNK = "0.1", fullname = "SDSS J000027.01+030715.5", ratext = "00h00m27.02s", dectext = "+03d07m15.6s", class = "RadioS", redshift = "2.353124", flag = "SPEC", redshift_ref = "2016SDSSD.C...0000:")
    @test isequal(d.flag, ["SPEC", missing, missing, missing, missing, missing, missing, missing, missing, "SPEC", missing, "SPEC", "SPEC", "SPEC", missing, "SPEC", "SPEC", "EST", "SPEC", missing, missing, missing, missing, missing, missing, missing, missing, missing, missing, missing, missing, missing, missing, missing, "SPEC", missing, "SPEC", missing, missing, missing, missing, missing, missing, "PHOT", missing, missing, missing, missing, missing, missing, missing, missing, "SPEC", "SPEC", "PHOT", missing, missing, missing, missing, missing])
    @test isconcretetype(eltype(d))

    e = FWT.read("cats_table.txt",
        [
            (cat=(1:5, String),name=(8:27, String),RA=(29:40, String),eRA=(43:47, Float64),Dec=(50:61, String),eDec=(63:67, String),freq=(69:76, Float64),flux=(78:86, Float64),flux_err=(88:95, Float64),_=(97:97, String)),
            (cat=(1:5, String),name=(8:28, String),RA=(30:41, String),eRA=(44:48, Float64),Dec=(51:62, String),eDec=(64:68, String),freq=(70:77, Float64),flux=(79:87, Float64),flux_err=(89:96, Float64),_=(98:98, String)),
        ];
        skiprows_startwith="#", restrict_remaining_chars=' ', missings=["n"])
    @test isequal(e[1], (cat="SPASS", name="SPASS_J000852+000015", RA="00 00 00", eRA=0.0, Dec="00 00 15.0", eDec=missing, freq=2307.0, flux=0.0802, flux_err=0.021, _="J"))
    @test isequal(e.eRA, [0.0, missing, missing, missing, missing, missing, missing, missing, missing, missing, missing, missing, missing])
    @test isconcretetype(eltype(e))
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
