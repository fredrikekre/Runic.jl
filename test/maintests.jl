# SPDX-License-Identifier: MIT

using Test: @test
using Runic: Runic

function maintests(f::R) where {R}

    bad = "1+1"
    good = "1 + 1\n"

    # Utils
    function cdtmp(f)
        return mktempdir(tmp -> cd(f, tmp))
    end
    function runic(std_in::String)
        return runic(String[], std_in)
    end
    function runic(argv::Vector{String} = String[], std_in::String = "")
        rc, stdout_str, stderr_str = mktemp() do stdin_path, stdin
            write(stdin_path, std_in)
            mktemp() do stdout_path, stdout
                mktemp() do stderr_path, stderr
                    rc = redirect_stdio(() -> f(copy(argv)); stdin, stdout, stderr)
                    close(stderr)
                    close(stdout)
                    return rc, read(stdout_path, String), read(stderr_path, String)
                end
            end
        end
        return rc, stdout_str, stderr_str
    end

    # runic --help
    let (rc, fd1, fd2) = runic(["--help"])
        @test rc == 0
        @test occursin("Runic.main - format Julia source code", fd1)
        @test isempty(fd2)
    end

    # runic --version
    let (rc, fd1, fd2) = runic(["--version"])
        @test rc == 0
        @test occursin("runic version $(Runic.RUNIC_VERSION), julia version $(VERSION)", fd1)
        @test isempty(fd2)
    end

    # runic <stdin >stdout
    for argv in [
            String[], ["-"],
            ["--output=-"], ["-o", "-"],
            ["--output=-", "-"], ["-o", "-", "-"],
        ]
        rc, fd1, fd2 = runic(argv, bad)
        @test rc == 0
        @test occursin(good, fd1)
        @test isempty(fd2)
    end

    # runic --output=out.jl <stdin
    cdtmp() do
        f_out = "out.jl"
        for argv in [
                ["--output=$f_out"], ["-o", f_out],
                ["--output=$f_out", "-"], ["-o", f_out, "-"],
            ]
            rm(f_out, force = true)
            rc, fd1, fd2 = runic(argv, bad)
            @test rc == 0
            @test isempty(fd1)
            @test isempty(fd2)
            @test read(f_out, String) == good
        end
    end

    # runic in.jl >stdout
    cdtmp() do
        f_in = "in.jl"
        write(f_in, bad)
        for argv in [[f_in], ["--output=-", f_in], ["-o", "-", f_in]]
            rc, fd1, fd2 = runic(argv)
            @test rc == 0
            @test occursin(good, fd1)
            @test isempty(fd2)
            @test read(f_in, String) == bad
        end
    end

    # runic --output=out.jl in.jl
    cdtmp() do
        f_in = "in.jl"
        write(f_in, bad)
        f_out = "out.jl"
        for argv in [["--output=$f_out", f_in], ["-o", f_out, f_in]]
            rm(f_out, force = true)
            rc, fd1, fd2 = runic(argv)
            @test rc == 0
            @test isempty(fd1) && isempty(fd2)
            @test read(f_out, String) == good
            @test read(f_in, String) == bad
        end
        # --verbose
        let argv = ["--verbose", "--output=$f_out", f_in]
            rm(f_out, force = true)
            rc, fd1, fd2 = runic(argv)
            @test rc == 0
            @test isempty(fd1)
            @test occursin("[1/1] Formatting `in.jl` -> `out.jl` ...", fd2)
            @test occursin("✔", fd2)
            @test !occursin("✖", fd2)
            @test read(f_out, String) == good
            @test read(f_in, String) == bad
        end
    end

    # runic --inplace in.jl (bad input)
    cdtmp() do
        f_in = "in.jl"
        for argv in [["--inplace", f_in], ["-i", f_in]]
            write(f_in, bad)
            rc, fd1, fd2 = runic(argv)
            @test rc == 0
            @test isempty(fd1) && isempty(fd2)
            @test read(f_in, String) == good
        end
        # --verbose
        let argv = ["-v", "--inplace", f_in]
            write(f_in, bad)
            rc, fd1, fd2 = runic(argv)
            @test rc == 0
            @test isempty(fd1)
            @test occursin("[1/1] Formatting `in.jl` ...", fd2)
            @test occursin("✔", fd2)
            @test !occursin("✖", fd2)
            @test read(f_in, String) == good
        end
    end

    # runic --inplace in.jl (good input)
    cdtmp() do
        f_in = "in.jl"
        for argv in [["--inplace", f_in], ["-i", f_in]]
            write(f_in, good)
            rc, fd1, fd2 = runic(argv)
            @test rc == 0
            @test isempty(fd1) && isempty(fd2)
            @test read(f_in, String) == good
        end
        # --verbose
        let argv = ["--verbose", "--inplace", f_in]
            write(f_in, good)
            rc, fd1, fd2 = runic(argv)
            @test rc == 0
            @test occursin("[1/1] Formatting `in.jl` ...", fd2)
            @test occursin("✔", fd2)
            @test !occursin("✖", fd2)
            @test read(f_in, String) == good
        end
    end

    # runic --inplace in/
    cdtmp() do
        fgood = "good.jl"
        mkdir("src")
        fbad = joinpath("src", "bad.jl")
        mkdir(".git")
        gitfile = joinpath(".git", "git.jl")
        write(gitfile, "this is not a Julia file")
        markdownfile = "markdown.md"
        write(markdownfile, "this is not a Julia file")
        for argv in [["--inplace", "."], ["-i", "."], ["-i", ".", "src"]]
            write(fgood, good)
            write(fbad, bad)
            rc, fd1, fd2 = runic(argv)
            @test rc == 0
            @test isempty(fd1) && isempty(fd2)
            @test read(fgood, String) == read(fbad, String) == good
        end
        # --verbose
        let argv = ["-v", "--inplace", "."]
            write(fgood, good)
            write(fbad, bad)
            rc, fd1, fd2 = runic(argv)
            @test rc == 0
            @test isempty(fd1)
            @test occursin("Formatting `good.jl` ...", fd2)
            @test occursin("Formatting `src/bad.jl` ...", fd2)
            @test occursin("[1/2]", fd2) && occursin("[2/2]", fd2)
            @test occursin("✔", fd2)
            @test !occursin("✖", fd2)
            @test !occursin("git.jl", fd2)
            @test !occursin("markdown.jl", fd2)
            @test read(fgood, String) == read(fbad, String) == good
        end
    end

    # runic --check in.jl (bad input)
    cdtmp() do
        f_in = "in.jl"
        for argv in [["--check", f_in], ["-c", f_in]]
            write(f_in, bad)
            rc, fd1, fd2 = runic(argv)
            @test rc == 1
            @test isempty(fd1) && isempty(fd2)
            @test read(f_in, String) == bad
        end
        # --verbose
        let argv = ["--verbose", "--check", f_in]
            write(f_in, bad)
            rc, fd1, fd2 = runic(argv)
            @test rc == 1
            @test isempty(fd1)
            @test occursin("[1/1] Checking `in.jl` ...", fd2)
            @test !occursin("✔", fd2)
            @test occursin("✖", fd2)
            @test read(f_in, String) == bad
        end
    end

    # runic --check in.jl (good input)
    cdtmp() do
        f_in = "in.jl"
        for argv in [["--check", f_in], ["-c", f_in]]
            write(f_in, good)
            rc, fd1, fd2 = runic(argv)
            @test rc == 0
            @test isempty(fd1) && isempty(fd2)
            @test read(f_in, String) == good
        end
        let argv = ["-v", "--check", f_in]
            write(f_in, good)
            rc, fd1, fd2 = runic(argv)
            @test rc == 0
            @test isempty(fd1)
            @test occursin("[1/1] Checking `in.jl` ...", fd2)
            @test occursin("✔", fd2)
            @test !occursin("✖", fd2)
            @test read(f_in, String) == good
        end
    end

    # runic --check in/
    cdtmp() do
        fgood = "good.jl"
        mkdir("src")
        fbad = joinpath("src", "bad.jl")
        mkdir(".git")
        gitfile = joinpath(".git", "git.jl")
        write(gitfile, "this is not a Julia file")
        markdownfile = "markdown.md"
        write(markdownfile, "this is not a Julia file")
        for argv in [["--check", "."], ["-c", "."]]
            write(fgood, good)
            write(fbad, bad)
            rc, fd1, fd2 = runic(argv)
            @test rc == 1
            @test isempty(fd1) && isempty(fd2)
            @test read(fgood, String) == good
            @test read(fbad, String) == bad
        end
        # --verbose
        let argv = ["--verbose", "--check", "."]
            write(fgood, good)
            write(fbad, bad)
            rc, fd1, fd2 = runic(argv)
            @test rc == 1
            @test isempty(fd1)
            @test occursin("Checking `good.jl` ...", fd2)
            @test occursin("Checking `src/bad.jl` ...", fd2)
            @test occursin("[1/2]", fd2) && occursin("[2/2]", fd2)
            @test occursin("✔", fd2)
            @test occursin("✖", fd2)
            @test !occursin("git.jl", fd2)
            @test !occursin("markdown.jl", fd2)
            @test read(fgood, String) == good
            @test read(fbad, String) == bad
        end
    end

    # runic --check --diff in.jl
    if Sys.which("git") !== nothing
        cdtmp() do
            f_in = "in.jl"
            for argv in [["--check", "--diff", f_in], ["-c", "-d", f_in]]
                write(f_in, bad)
                rc, fd1, fd2 = runic(argv)
                @test rc == 1
                @test isempty(fd1)
                @test !occursin("Checking `in.jl` ...", fd2)
                @test !occursin("✔", fd2)
                @test !occursin("✖", fd2)
                @test occursin("diff --git", fd2)
                @test occursin("-1+1", fd2)
                @test occursin("+1 + 1", fd2)
                @test read(f_in, String) == bad
            end
            let argv = ["-v", "--check", "--diff", f_in]
                write(f_in, bad)
                rc, fd1, fd2 = runic(argv)
                @test rc == 1
                @test isempty(fd1)
                @test occursin("[1/1] Checking `in.jl` ...", fd2)
                @test !occursin("✔", fd2)
                @test occursin("✖", fd2)
                @test occursin("diff --git", fd2)
                @test occursin("-1+1", fd2)
                @test occursin("+1 + 1", fd2)
                @test read(f_in, String) == bad
            end
        end
    end

    # runic --verbose
    cdtmp() do
        f_in = "in.jl"
        write(f_in, good)
        let argv = ["--verbose"; "--check"; fill(f_in, 10)]
            rc, fd1, fd2 = runic(argv)
            @test rc == 0
            @test isempty(fd1)
            for i in 1:9
                @test occursin("[ $(i)/10] Checking `in.jl` ...", fd2)
            end
            @test occursin("[10/10] Checking `in.jl` ...", fd2)
        end
    end

    # Error paths
    # runic -o
    let (rc, fd1, fd2) = runic(["-o"])
        @test rc == 1
        @test isempty(fd1)
        @test occursin("expected output file argument after `-o`", fd2)
    end

    # runic in.jl -
    let (rc, fd1, fd2) = runic(["in.jl", "-"])
        @test rc == 1
        @test isempty(fd1)
        @test occursin("input `-` can not be used with multiple files", fd2)
    end

    # runic --inplace --check (TODO: perhaps this should be allowed?)
    let (rc, fd1, fd2) = runic(["--inplace", "--check"])
        @test rc == 1
        @test isempty(fd1)
        @test occursin("options `--inplace` and `--check` are mutually exclusive", fd2)
    end

    # runic --inplace --output=out.jl in.jl
    let (rc, fd1, fd2) = runic(["--inplace", "--output=out.jl", "in.jl"])
        @test rc == 1
        @test isempty(fd1)
        @test occursin("options `--inplace` and `--output` are mutually exclusive", fd2)
    end

    # runic --check --output=out.jl in.jl
    let (rc, fd1, fd2) = runic(["--check", "--output=out.jl", "in.jl"])
        @test rc == 1
        @test isempty(fd1)
        @test occursin("options `--check` and `--output` are mutually exclusive", fd2)
    end

    # runic --inplace
    let (rc, fd1, fd2) = runic(["--inplace"])
        @test rc == 1
        @test isempty(fd1)
        @test occursin("option `--inplace` can not be used together with stdin input", fd2)
    end

    # runic --output=out.jl in1.jl in2.jl
    let (rc, fd1, fd2) = runic(["--output=out.jl", "in1.jl", "in2.jl"])
        @test rc == 1
        @test isempty(fd1)
        @test occursin("option `--output` can not be used together with multiple input files", fd2)
    end

    # runic in1.jl in2.jl
    let (rc, fd1, fd2) = runic(["in1.jl", "in2.jl"])
        @test rc == 1
        @test isempty(fd1)
        @test occursin("option `--inplace` or `--check` required with multiple input files", fd2)
    end

    # runic --diff (with no git)
    let (rc, fd1, fd2) = withenv(() -> runic(["--diff"]), "PATH" => "")
        @test rc == 1
        @test isempty(fd1)
        @test occursin("option `--diff` requires `git` to be installed", fd2)
    end

    # runic in.jl (not readable)
    cdtmp() do
        f_in = "in.jl"
        write(f_in, bad)
        omode = filemode(f_in)
        chmod(f_in, omode & (typemax(omode) ⊻ 0o444))
        rc, fd1, fd2 = runic([f_in])
        chmod(f_in, omode)
        @test rc == 1
        @test isempty(fd1)
        @test occursin("could not read input from file", fd2)
        @test occursin("SystemError: opening file", fd2)
    end

    # runic doesntexist.jl
    cdtmp() do
        rc, fd1, fd2 = runic(["doesntexist.jl"])
        @test rc == 1
        @test isempty(fd1)
        @test occursin("input file does not exist", fd2)
    end

    # runic -o in.jl in.jl
    cdtmp() do
        f_in = "in.jl"
        write(f_in, bad)
        rc, fd1, fd2 = runic(["-o", f_in, f_in])
        @test rc == 1
        @test isempty(fd1)
        @test occursin("can not use same file for input and output", fd2)
    end

    # runic --check unparseable.jl
    cdtmp() do
        f_in = "in.jl"
        write(f_in, "syntax error")
        rc, fd1, fd2 = runic(["--check", f_in])
        @test rc == 1
        @test isempty(fd1)
        @test occursin("failed to parse input", fd2)
    end

    # runic -o readonly.jl in.jl
    cdtmp() do
        f_in = "in.jl"
        write(f_in, bad)
        f_out = "readonly.jl"
        touch(f_out)
        omode = filemode(f_out)
        chmod(f_out, omode & (typemax(omode) ⊻ 0o222))
        rc, fd1, fd2 = runic(["-o", f_out, f_in])
        chmod(f_out, omode)
        @test rc == 1
        @test isempty(fd1)
        @test occursin("could not write to output file", fd2)
    end

    # runic --lines
    cdtmp() do
        src = """
        function f(a,b)
            return a+b
         end
        """
        rc, fd1, fd2 = runic(["--lines=1:1"], src)
        @test rc == 0 && isempty(fd2)
        @test fd1 == "function f(a, b)\n    return a+b\n end\n"
        rc, fd1, fd2 = runic(["--lines=2:2"], src)
        @test rc == 0 && isempty(fd2)
        @test fd1 == "function f(a,b)\n    return a + b\n end\n"
        rc, fd1, fd2 = runic(["--lines=3:3"], src)
        @test rc == 0 && isempty(fd2)
        @test fd1 == "function f(a,b)\n    return a+b\nend\n"
        rc, fd1, fd2 = runic(["--lines=1:1", "--lines=3:3"], src)
        @test rc == 0 && isempty(fd2)
        @test fd1 == "function f(a, b)\n    return a+b\nend\n"
        rc, fd1, fd2 = runic(["--lines=1:1", "--lines=2:2", "--lines=3:3"], src)
        @test rc == 0 && isempty(fd2)
        @test fd1 == "function f(a, b)\n    return a + b\nend\n"
        rc, fd1, fd2 = runic(["--lines=1:2"], src)
        @test rc == 0 && isempty(fd2)
        @test fd1 == "function f(a, b)\n    return a + b\n end\n"
        # Errors
        rc, fd1, fd2 = runic(["--lines=1:2", "--lines=2:3"], src)
        @test rc == 1
        @test isempty(fd1)
        @test occursin("`--lines` ranges cannot overlap", fd2)
        rc, fd1, fd2 = runic(["--lines=0:1"], src)
        @test rc == 1 && isempty(fd1)
        @test occursin("`--lines` range out of bounds", fd2)
        rc, fd1, fd2 = runic(["--lines=3:4"], src)
        @test rc == 1 && isempty(fd1)
        @test occursin("`--lines` range out of bounds", fd2)
        rc, fd1, fd2 = runic(["--lines=3:4", "foo.jl", "bar.jl"], src)
        @test rc == 1 && isempty(fd1)
        @test occursin("option `--lines` can not be used together with multiple input files", fd2)
    end

    return
end

# rc = let argv = pushfirst!(copy(argv), "runic"), argc = length(argv) % Cint
#     GC.@preserve argv begin
#         argvptr = Base.unsafe_convert(Ptr{Ptr{UInt8}}, Base.cconvert(Ptr{Ptr{UInt8}}, argv))
#         redirect_stdio(() -> Main.RunicC.main(argc, argvptr); stdin, stdout, stderr)
#     end
# end
