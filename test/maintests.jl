# SPDX-License-Identifier: MIT

using Test: @test
using Runic: Runic

# Entrypoint which points `homedir()` to a temporary directory for the duration of the
# tests so that Runic never operates on the real home directory, even if a test (or Runic
# itself) misbehaves. If `homedir()` can not be redirected through the environment the
# tests still run: `assert_not_real_home` below guards every runic invocation either way.
function maintests(f::R) where {R}
    real_home = try
        realpath(homedir())
    catch
        ""
    end
    mktempdir() do fake_home
        env_keys = ["HOME", "USERPROFILE", "HOMEDRIVE", "HOMEPATH"]
        old_env = [get(ENV, k, nothing) for k in env_keys]
        try
            ENV["HOME"] = ENV["USERPROFILE"] = fake_home
            delete!(ENV, "HOMEDRIVE")
            delete!(ENV, "HOMEPATH")
            maintests(f, real_home)
        finally
            for (k, v) in zip(env_keys, old_env)
                v === nothing ? delete!(ENV, k) : (ENV[k] = v)
            end
        end
    end
    return
end

function maintests(f::R, real_home::String) where {R}

    bad = "1+1"
    good = "1 + 1\n"

    # Utils
    function cdtmp(f)
        return mktempdir(tmp -> cd(f, tmp))
    end
    # Safety net: refuse to invoke runic if any input path resolves to the real home
    # directory (or its parent). The tests run `runic --inplace` so a bug in Runic's
    # directory protection could otherwise be disastrous. Note that `realpath` resolves
    # relative paths (e.g. `.`) against the current working directory.
    function assert_not_real_home(argv::Vector{String})
        isempty(real_home) && return
        for p in argv
            startswith(p, "-") && continue # skip options (and stdin input `-`)
            rp = try
                realpath(p)
            catch
                continue
            end
            if rp == real_home || rp == dirname(real_home)
                error("tests must not run runic on the real home directory: `$(p)`")
            end
        end
        return
    end
    function runic(std_in::String)
        return runic(String[], std_in)
    end
    function runic(argv::Vector{String} = String[], std_in::String = "")
        assert_not_real_home(argv)
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

    # runic --stdin-filename <stdin >stdout
    let (rc, fd1, fd2) = runic(["--stdin-filename=/foo/bar/baz.jl"], "a+")
        @test rc == 1
        @test isempty(fd1)
        @test occursin("failed to parse input from /foo/bar/baz.jl", fd2)
    end
    let (rc, fd1, fd2) = runic(String[], "a+")
        @test rc == 1
        @test isempty(fd1)
        @test occursin("failed to parse input from stdin", fd2)
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
            @test occursin("[1/1] Formatting `in.jl` ...", fd2)
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

    # Same-width byte replacements (tab to space) must still count as changes.
    let (rc, fd1, fd2) = runic(["--check"], "x\t= 1\n")
        @test rc == 1
        @test isempty(fd1) && isempty(fd2)
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

    # runic emptydir/
    cdtmp() do
        for argv in [["-i", "."], ["-c", "."], ["-c", "-v", "."], ["-c", "-d", "."]]
            rc, fd1, fd2 = runic(["-i", "."])
            @test rc == 0
            @test isempty(fd1) && isempty(fd2)
        end
        let (rc, fd1, fd2) = runic(["."])
            @test rc == 1
            @test isempty(fd1)
            @test occursin("option `--inplace` or `--check` required with multiple", fd2)
        end
    end

    # Error paths
    # runic -o
    let (rc, fd1, fd2) = runic(["-o"])
        @test rc == 1
        @test isempty(fd1)
        @test occursin("expected output file argument after `-o`", fd2)
    end

    # - only allowed once and only in first position
    cdtmp() do
        for argv in [[".", "-"], ["-", "."], ["in.jl", "-"], ["-", "in.jl"]]
            rc, fd1, fd2 = runic(argv)
            @test rc == 1
            @test isempty(fd1)
            @test occursin("input `-` can not be combined with other input", fd2)
        end
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
        let (rc, fd1, fd2) = runic([f_in])
            @test rc == 1
            @test isempty(fd1)
            @test !occursin("Formatting", fd2)
            @test occursin("could not read input from file", fd2)
            @test occursin("SystemError: opening file", fd2)
        end
        let (rc, fd1, fd2) = runic(["-v", "-i", f_in])
            @test rc == 1
            @test isempty(fd1)
            @test occursin("[1/1] Formatting `in.jl` ...", fd2)
            @test occursin("could not read input from file", fd2)
            @test occursin("SystemError: opening file", fd2)
        end
        chmod(f_in, omode)
    end

    # runic doesntexist.jl doesntexist/
    cdtmp() do
        nofile = "doesntexist.jl"
        nodir = "doesntexist.jl"
        let (rc, fd1, fd2) = runic(["-c", nofile, nodir])
            @test rc == 1
            @test isempty(fd1)
            @test !occursin("Formatting", fd2)
            @test occursin("input path is not a file or directory: `$nofile`", fd2)
            @test occursin("input path is not a file or directory: `$nodir`", fd2)
        end
        let (rc, fd1, fd2) = runic(["-v", "-c", nofile, nodir])
            @test rc == 1
            @test isempty(fd1)
            @test occursin("[1/2] Checking `$nofile` ...", fd2)
            @test occursin("input path is not a file or directory: `$nofile`", fd2)
            @test occursin("[2/2] Checking `$nodir` ...", fd2)
            @test occursin("input path is not a file or directory: `$nodir`", fd2)
        end
    end

    # runic -o in.jl in.jl
    cdtmp() do
        f_in = "in.jl"
        write(f_in, bad)
        let (rc, fd1, fd2) = runic(["-o", f_in, f_in])
            @test rc == 1
            @test isempty(fd1)
            @test !occursin("Formatting", fd2)
            @test occursin("can not use same file for input and output", fd2)
        end
        let (rc, fd1, fd2) = runic(["-o", f_in, "-v", f_in])
            @test rc == 1
            @test isempty(fd1)
            @test occursin("[1/1] Formatting `in.jl` ...", fd2)
            @test occursin("can not use same file for input and output", fd2)
        end
    end

    # runic --check unparseable.jl
    cdtmp() do
        f_in = "in.jl"
        write(f_in, "syntax error")
        rc, fd1, fd2 = runic(["--check", f_in])
        @test rc == 1
        @test isempty(fd1)
        @test occursin("failed to parse input from in.jl: ", fd2)
        # TODO: Not juliac-compatible
        # @test occursin("Error @ in.jl:1:7", fd2) # Relies on JuliaSyntax output
    end

    # runic --check < unparseable.jl
    cdtmp() do
        rc, fd1, fd2 = runic(["--check"], "syntax error")
        @test rc == 1
        @test isempty(fd1)
        @test occursin("failed to parse input from stdin: ", fd2)
        # TODO: Not juliac-compatible
        # @test occursin("Error @ stdin:1:7", fd2) # Relies on JuliaSyntax output
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
        # Marker text in ordinary source is not mistaken for an inserted marker.
        marker_src = "b = $(repr(Runic.RANGE_FORMATTING_BEGIN))\nx=1\n"
        rc, fd1, fd2 = runic(["--lines=2:2"], marker_src)
        @test rc == 0 && isempty(fd2)
        @test fd1 == "b = $(repr(Runic.RANGE_FORMATTING_BEGIN))\nx = 1\n"
        # Standalone user comments matching the default markers must also be preserved.
        marker_comment_src = string(
            Runic.RANGE_FORMATTING_BEGIN, "\nx=1\n",
            Runic.RANGE_FORMATTING_END, "\ny=2\n",
        )
        rc, fd1, fd2 = runic(["--lines=4:4"], marker_comment_src)
        @test rc == 0 && isempty(fd2)
        @test fd1 == string(
            Runic.RANGE_FORMATTING_BEGIN, "\nx=1\n",
            Runic.RANGE_FORMATTING_END, "\ny = 2\n",
        )
        # Errors
        rc, fd1, fd2 = runic(["--lines=1:2", "--lines=2:3"], src)
        @test rc == 1
        @test isempty(fd1)
        @test occursin("`--lines` ranges cannot overlap", fd2)
        rc, fd1, fd2 = runic(["--lines=0:1"], src)
        @test rc == 1 && isempty(fd1)
        @test occursin("`--lines` range out of bounds", fd2)
        rc, fd1, fd2 = runic(["--lines=3:5"], src)
        @test rc == 1 && isempty(fd1)
        @test occursin("`--lines` range out of bounds", fd2)
        rc, fd1, fd2 = runic(["--lines=$(typemax(Int))0:$(typemax(Int))0"], src)
        @test rc == 1 && isempty(fd1)
        @test occursin("can not parse `--lines` argument as an integer range", fd2)
        rc, fd1, fd2 = runic(["--lines=3:4", "."])
        @test rc == 1 && isempty(fd1)
        @test occursin("option `--lines` can not be used together with multiple input files", fd2)
        # --diff and --lines: no comment markers in diff output
        if Sys.which("git") !== nothing
            rc, fd1, fd2 = runic(["--lines=1:1", "--diff"], src)
            @test rc == 0
            @test fd1 == "function f(a, b)\n    return a+b\n end\n"
            @test occursin("-function f(a,b)", fd2)
            @test occursin("+function f(a, b)", fd2)
            @test !occursin(Runic.RANGE_FORMATTING_BEGIN, fd2)
            @test !occursin(Runic.RANGE_FORMATTING_END, fd2)
        end
    end
    # An empty input still has the virtual first line accepted by editors/tooling.
    let (rc, fd1, fd2) = runic(["--lines=1:1"], "")
        @test rc == 0
        @test fd1 == "\n" && isempty(fd2)
    end
    let (rc, fd1, fd2) = runic(["--lines=2:2"], "")
        @test rc == 1 && isempty(fd1)
        @test occursin("`--lines` range out of bounds", fd2)
        @test !occursin("BoundsError", fd2)
    end

    # runic --docstrings
    let src = "\"\"\"\n```julia\nx=1\n```\n\"\"\"\nfunction foo()\nend\n",
            expected = "\"\"\"\n```julia\nx = 1\n```\n\"\"\"\nfunction foo()\nend\n"
        rc, fd1, fd2 = runic(String[], src)
        @test rc == 0 && fd1 == src  # no change without --docstrings
        rc, fd1, fd2 = runic(["--docstrings"], src)
        @test rc == 0 && fd1 == expected
        @test isempty(fd2)
    end

    # runic Markdown stdin dispatch via --stdin-filename
    let src_md = "```julia\nx=1\n```\n",
            expected_md = "```julia\nx = 1\n```\n"
        # Default (no stdin-filename) treats input as Julia — this one would ParseError
        # since the source contains a code fence. Confirm with a simple source instead.
        rc, fd1, fd2 = runic(["--stdin-filename=foo.md"], src_md)
        @test rc == 0
        @test fd1 == expected_md
        @test isempty(fd2)
        # Without .md extension, falls back to Julia (backticks parse as Cmd literal, no change)
        rc, fd1, fd2 = runic(["--stdin-filename=foo.jl"], src_md)
        @test rc == 0
        @test fd1 == src_md
    end
    let src_md = "```julia\r\nx=1\r\n```\r\n"
        rc, fd1, fd2 = runic(["--stdin-filename=foo.md"], src_md)
        @test rc == 0 && isempty(fd2)
        @test fd1 == "```julia\r\nx = 1\r\n```\r\n"
    end

    # runic Quarto markdown stdin dispatch via --stdin-filename=*.qmd
    let src_qmd = "```{julia}\n#| echo: false\nx=1\n```\n",
            expected_qmd = "```{julia}\n#| echo: false\nx = 1\n```\n"
        rc, fd1, fd2 = runic(["--stdin-filename=foo.qmd"], src_qmd)
        @test rc == 0
        @test fd1 == expected_qmd
        @test isempty(fd2)
    end

    # runic --extensions for directory walking
    mktempdir() do dir
        write(joinpath(dir, "a.jl"), "x=1\n")
        write(joinpath(dir, "b.md"), "```julia\nx=1\n```\n")

        # Default (only .jl): check finds the .jl file unformatted, ignores .md
        rc, fd1, fd2 = runic(["--check", dir])
        @test rc == 1  # .jl needs formatting

        # Format .jl, leave .md alone
        rc, fd1, fd2 = runic(["-i", dir])
        @test rc == 0
        @test read(joinpath(dir, "a.jl"), String) == "x = 1\n"
        @test read(joinpath(dir, "b.md"), String) == "```julia\nx=1\n```\n"  # untouched

        # Restore mangled input for the next leg
        write(joinpath(dir, "a.jl"), "x=1\n")

        # Extension filter for .md only
        rc, fd1, fd2 = runic(["--extensions=md", "-i", dir])
        @test rc == 0
        @test read(joinpath(dir, "a.jl"), String) == "x=1\n"  # untouched
        @test read(joinpath(dir, "b.md"), String) == "```julia\nx = 1\n```\n"

        # Both extensions
        write(joinpath(dir, "b.md"), "```julia\nx=1\n```\n")  # re-mangle
        rc, fd1, fd2 = runic(["--extensions=jl,md", "-i", dir])
        @test rc == 0
        @test read(joinpath(dir, "a.jl"), String) == "x = 1\n"
        @test read(joinpath(dir, "b.md"), String) == "```julia\nx = 1\n```\n"
    end

    # Explicit .md path bypasses the extension filter (format it regardless)
    mktempdir() do dir
        md = joinpath(dir, "doc.md")
        write(md, "```julia\nx=1\n```\n")
        rc, fd1, fd2 = runic(["-i", md])
        @test rc == 0
        @test read(md, String) == "```julia\nx = 1\n```\n"
    end

    # --lines works with Markdown (block-granular overlap)
    # Two blocks in the source; range overlaps only the second → only that one formatted.
    let src = "```julia\nx=1\n```\n\n```julia\ny=2\n```\n",
            expected = "```julia\nx=1\n```\n\n```julia\ny = 2\n```\n"
        # Second block occupies lines 5..7; selecting line 6 overlaps that block only.
        rc, fd1, fd2 = runic(["--lines=6:6", "--stdin-filename=foo.md"], src)
        @test rc == 0
        @test fd1 == expected
    end
    # Range entirely in prose → no change
    let src = "prose line\n\n```julia\nx=1\n```\n\nmore prose\n"
        rc, fd1, fd2 = runic(["--lines=1:1", "--stdin-filename=foo.md"], src)
        @test rc == 0
        @test fd1 == src
    end
    # Range partially crossing a block → whole block formatted (block-granular rule)
    let src = "prose\n\n```julia\nx=1\ny=2\n```\n",
            expected = "prose\n\n```julia\nx = 1\ny = 2\n```\n"
        # Range 1..4 covers the prose line + fence opener + first content line; block
        # starts at line 3. Any overlap → full block formatted.
        rc, fd1, fd2 = runic(["--lines=1:4", "--stdin-filename=foo.md"], src)
        @test rc == 0
        @test fd1 == expected
    end
    # Markdown inputs validate ranges just like Julia inputs.
    let src = "```julia\nx=1\n```\n"
        for range in ("0:1", "5:5")
            rc, fd1, fd2 =
                runic(["--lines=$range", "--stdin-filename=foo.md"], src)
            @test rc == 1 && isempty(fd1)
            @test occursin("`--lines` range out of bounds", fd2)
        end
    end

    # --check for Markdown: returns 1 when reformatting would change the file
    let (rc, fd1, fd2) = runic(["--check", "--stdin-filename=foo.md"], "```julia\nx=1\n```\n")
        @test rc == 1
    end
    # --check for Markdown: returns 0 when already formatted
    let (rc, fd1, fd2) = runic(["--check", "--stdin-filename=foo.md"], "```julia\nx = 1\n```\n")
        @test rc == 0
    end

    # --diff for Markdown produces a diff
    let (rc, fd1, fd2) = runic(["--diff", "--stdin-filename=foo.md"], "```julia\nx=1\n```\n")
        @test rc == 0
        @test occursin("-x=1", fd2) || occursin("x=1", fd2)
        @test occursin("+x = 1", fd2) || occursin("x = 1", fd2)
    end

    # --extensions with invalid (empty) input
    cdtmp() do
        rc, fd1, fd2 = runic(["--extensions=", "."])
        @test rc != 0
    end

    # Safety checks for directory recursion (see #200)

    # Temporarily point `homedir()` to `home` and, if given, replace `DEPOT_PATH` with
    # `depot`. Returns `false` (and does nothing but warn) if `homedir()` can not be
    # controlled through the environment on this system.
    function withhome(f, home::String; depot::Union{String, Nothing} = nothing)
        keys = ["HOME", "USERPROFILE", "HOMEDRIVE", "HOMEPATH"]
        old = [get(ENV, k, nothing) for k in keys]
        old_depot_path = copy(Base.DEPOT_PATH)
        try
            ENV["HOME"] = ENV["USERPROFILE"] = home
            delete!(ENV, "HOMEDRIVE")
            delete!(ENV, "HOMEPATH")
            if realpath(homedir()) != realpath(home)
                @warn "`homedir()` can not be controlled through the environment on " *
                    "this system, skipping tests that depend on the home directory"
                return false
            end
            if depot !== nothing
                push!(empty!(Base.DEPOT_PATH), depot)
            end
            f()
            return true
        finally
            for (k, v) in zip(keys, old)
                v === nothing ? delete!(ENV, k) : (ENV[k] = v)
            end
            append!(empty!(Base.DEPOT_PATH), old_depot_path)
        end
    end

    # Nested git repositories are skipped, the root one is not
    cdtmp() do
        mkdir(".git")
        mkpath(joinpath("sub", ".git"))
        # The contents of `.git` folders are never collected, not even with
        # --recurse/--force-recurse
        rootgitfile = joinpath(".git", "git.jl")
        write(rootgitfile, "this is not a Julia file")
        subgitfile = joinpath("sub", ".git", "git.jl")
        write(subgitfile, "this is not a Julia file")
        f_root = "root.jl"
        f_sub = joinpath("sub", "sub.jl")
        write(f_root, bad)
        write(f_sub, bad)
        rc, fd1, fd2 = runic(["-i", "."])
        @test rc == 0
        @test read(f_root, String) == good
        @test read(f_sub, String) == bad # untouched
        @test isempty(fd2) # skipped silently
        # ... unless --recurse is passed
        rc, fd1, fd2 = runic(["--recurse", "-i", "."])
        @test rc == 0
        @test read(f_root, String) == read(f_sub, String) == good
        @test isempty(fd2)
        # --force-recurse implies --recurse
        write(f_sub, bad)
        rc, fd1, fd2 = runic(["--force-recurse", "-i", "."])
        @test rc == 0
        @test read(f_sub, String) == good
        @test isempty(fd2)
        # ... but not even --recurse/--force-recurse collect files inside `.git` folders
        # (rc would be 1 from the parse failure if these files were collected)
        @test read(rootgitfile, String) == "this is not a Julia file"
        @test read(subgitfile, String) == "this is not a Julia file"
        # A `.git` file (submodule, linked worktree) also marks a nested repository
        rm(joinpath("sub", ".git"), recursive = true)
        write(joinpath("sub", ".git"), "gitdir: ../.git/modules/sub\n")
        write(f_sub, bad)
        rc, fd1, fd2 = runic(["-i", "."])
        @test rc == 0
        @test read(f_sub, String) == bad # untouched
        # Directories listed explicitly on the command line are walk roots and thus
        # exempt from the nested git repository check
        rc, fd1, fd2 = runic(["-i", "sub"])
        @test rc == 0
        @test read(f_sub, String) == good
        write(f_sub, bad)
        rc, fd1, fd2 = runic(["-i", ".", "sub"])
        @test rc == 0
        @test read(f_root, String) == read(f_sub, String) == good
        # ... and the order of the arguments does not matter
        write(f_root, bad)
        write(f_sub, bad)
        rc, fd1, fd2 = runic(["-i", "sub", "."])
        @test rc == 0
        @test read(f_root, String) == read(f_sub, String) == good
        @test isempty(fd2)
    end

    # Refuse to recurse into the home directory
    cdtmp() do
        home = mkdir("home")
        f_home = joinpath(home, "home.jl")
        write(f_home, bad)
        withhome(abspath(home)) do
            # Explicitly passing the home directory, and `.` from within it, is an error
            for (dir, argv) in [(".", ["-i", home]), (home, ["-i", "."])]
                write(f_home, bad)
                rc, fd1, fd2 = cd(() -> runic(argv), dir)
                @test rc == 1
                @test read(f_home, String) == bad # untouched
                @test occursin("refusing to recurse into", fd2)
                @test occursin("--force-recurse", fd2)
            end
            # ... and --recurse does not override it (only --force-recurse does)
            rc, fd1, fd2 = runic(["--recurse", "-i", home])
            @test rc == 1
            @test read(f_home, String) == bad # untouched
            @test occursin("refusing to recurse into", fd2)
            # ... unless --force-recurse is passed
            rc, fd1, fd2 = runic(["--force-recurse", "-i", home])
            @test rc == 0
            @test read(f_home, String) == good
            # A home directory nested below the input path is also an error (e.g.
            # `runic -i /` should error when the walk reaches the home directory), also
            # with --recurse
            write(f_home, bad)
            f_other = "other.jl"
            write(f_other, bad)
            for argv in [["-i", "."], ["--recurse", "-i", "."]]
                rc, fd1, fd2 = runic(argv)
                @test rc == 1
                @test occursin("refusing to recurse into", fd2)
                @test read(f_home, String) == bad # untouched
                @test read(f_other, String) == bad # untouched (the run is aborted)
            end
            # ... unless --force-recurse is passed
            rc, fd1, fd2 = runic(["--force-recurse", "-i", "."])
            @test rc == 0
            @test read(f_home, String) == read(f_other, String) == good
        end
    end

    # Refuse to recurse into the depot and its immediate subdirectories
    cdtmp() do
        home = abspath(mkdir("home"))
        depot = mkpath(joinpath(home, ".julia"))
        f_depot = joinpath(depot, "depot.jl")
        write(f_depot, bad)
        pkgdir = mkpath(joinpath(depot, "packages", "Foo", "abc123"))
        f_pkg = joinpath(pkgdir, "pkg.jl")
        write(f_pkg, bad)
        devdir = mkpath(joinpath(depot, "dev", "Foo"))
        f_dev = joinpath(devdir, "dev.jl")
        write(f_dev, bad)
        withhome(home) do
            # The depot itself, and `packages`/`dev`/... below it
            for dir in [depot, joinpath(depot, "packages"), joinpath(depot, "dev")]
                rc, fd1, fd2 = runic(["-i", dir])
                @test rc == 1
                @test occursin("refusing to recurse into", fd2)
                @test occursin("--force-recurse", fd2)
            end
            @test read(f_depot, String) == read(f_pkg, String) == read(f_dev, String) == bad
            # Development checkouts below `~/.julia/dev` are still formatted
            rc, fd1, fd2 = runic(["-i", devdir])
            @test rc == 0
            @test read(f_dev, String) == good
            # ... and so is a package in `~/.julia/packages` if requested explicitly
            rc, fd1, fd2 = runic(["-i", pkgdir])
            @test rc == 0
            @test read(f_pkg, String) == good
            # --force-recurse overrides
            rc, fd1, fd2 = runic(["--force-recurse", "-i", depot])
            @test rc == 0
            @test read(f_depot, String) == good
        end
    end

    # Depots configured via `DEPOT_PATH` are also protected (not just the default
    # `~/.julia` under the home directory)
    cdtmp() do
        home = abspath(mkdir("home"))
        depot = abspath(mkpath(joinpath("custom", "depot")))
        f_depot = joinpath(depot, "depot.jl")
        write(f_depot, bad)
        pkgdir = mkpath(joinpath(depot, "packages", "Foo", "abc123"))
        f_pkg = joinpath(pkgdir, "pkg.jl")
        write(f_pkg, bad)
        withhome(home; depot = depot) do
            # The depot itself and its immediate subdirectories are refused as input paths
            for dir in [depot, joinpath(depot, "packages")]
                rc, fd1, fd2 = runic(["-i", dir])
                @test rc == 1
                @test occursin("refusing to recurse into", fd2)
            end
            @test read(f_depot, String) == read(f_pkg, String) == bad
            # ... and result in an error when reached recursively
            rc, fd1, fd2 = runic(["-i", "custom"])
            @test rc == 1
            @test occursin("refusing to recurse into", fd2)
            @test read(f_depot, String) == bad
            # Deeper directories can still be formatted
            rc, fd1, fd2 = runic(["-i", pkgdir])
            @test rc == 0
            @test read(f_pkg, String) == good
            # --force-recurse overrides
            rc, fd1, fd2 = runic(["--force-recurse", "-i", depot])
            @test rc == 0
            @test read(f_depot, String) == good
        end
    end

    return
end

# rc = let argv = pushfirst!(copy(argv), "runic"), argc = length(argv) % Cint
#     GC.@preserve argv begin
#         argvptr = Base.unsafe_convert(Ptr{Ptr{UInt8}}, Base.cconvert(Ptr{Ptr{UInt8}}, argv))
#         redirect_stdio(() -> Main.RunicC.main(argc, argvptr); stdin, stdout, stderr)
#     end
# end
