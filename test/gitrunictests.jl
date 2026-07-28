# SPDX-License-Identifier: MIT

module GitRunicTests

using Runic
using Test

const GitRunic = Runic.GitRunic

function with_repository(f; commit::Bool = true)
    return mktempdir() do dir
        cd(dir) do
            GitRunic.git(["init", "-q"])
            GitRunic.git(["config", "user.email", "runic@example.com"])
            GitRunic.git(["config", "user.name", "Runic Tests"])
            GitRunic.git(["config", "core.autocrlf", "false"])
            if commit
                write("base.jl", "base = 1\n")
                GitRunic.git(["add", "base.jl"])
                GitRunic.git(["commit", "-qm", "initial"])
            end
            return f()
        end
    end
end

@testset "GitRunic" begin
    @testset "Git-quoted paths" begin
        with_repository() do
            GitRunic.git(["config", "core.quotePath", "true"])
            filenames = [
                "space name.jl",
                "λ.jl",
            ]
            if Sys.isunix()
                append!(filenames, ["tab\tname.jl", "line\nname.jl"])
            end
            for filename in filenames
                write(filename, "x = 1\n")
            end
            GitRunic.git(["add", "."])
            GitRunic.git(["commit", "-qm", "add unusual paths"])
            for filename in filenames
                write(filename, "x=2\n")
            end

            patch, status = GitRunic.compute_diff(["HEAD"], String[], false, false)
            @test status == 0
            @test Set(keys(GitRunic.extract_lines(patch))) == Set(filenames)

            @test GitRunic._main(["--force", "--quiet"]) == 1
            for filename in filenames
                @test read(filename, String) == "x = 2\n"
            end
        end
    end

    @testset "diff-looking source lines are not headers" begin
        patch = """
        diff --git a/x.jl b/x.jl
        --- a/x.jl
        +++ b/x.jl
        @@ -1 +1 @@
        -old
        +++ b/not-a-header.jl
        @@ -3 +3 @@
        -also_old
        +also_new
        """
        @test GitRunic.extract_lines(patch) == Dict("x.jl" => [1:1, 3:3])
    end

    @testset "staged formatting updates only the index" begin
        with_repository() do
            write("base.jl", "base=2\n")
            GitRunic.git(["add", "base.jl"])
            write("base.jl", "base=3\n")

            @test GitRunic._main(["--staged", "--quiet"]) == 1
            @test GitRunic.git(["show", ":base.jl"]) == "base = 2"
            @test read("base.jl", String) == "base=3\n"
            @test GitRunic.git(["status", "--short", "--", "base.jl"]) == "MM base.jl"
        end
    end

    @testset "untracked files remain untouched" begin
        with_repository() do
            write("base.jl", "base=2\n")
            write("untracked.jl", "untracked=2\n")

            @test GitRunic._main(["--force", "--quiet"]) == 1
            @test read("base.jl", String) == "base = 2\n"
            @test read("untracked.jl", String) == "untracked=2\n"
        end
    end

    @testset "Markdown extension matching is case-insensitive" begin
        with_repository() do
            write("README.MD", "```julia\nx = 1\n```\n")
            GitRunic.git(["add", "README.MD"])
            GitRunic.git(["commit", "-qm", "add Markdown"])
            write("README.MD", "```julia\nx=2\n```\n")
            GitRunic.git(["add", "README.MD"])

            @test GitRunic._main(["--staged", "--quiet", "--extensions=md"]) == 1
            @test GitRunic.git(["show", ":README.MD"]) == "```julia\nx = 2\n```"
        end
    end

    @testset "staged formatting uses index file types" begin
        if Sys.isunix()
            with_repository() do
                write("base.jl", "base=2\n")
                GitRunic.git(["add", "base.jl"])
                rm("base.jl")
                write("outside", "outside")
                symlink("outside", "base.jl")

                @test GitRunic._main(["--staged", "--quiet"]) == 1
                @test GitRunic.git(["show", ":base.jl"]) == "base = 2"
                @test islink("base.jl")
                @test readlink("base.jl") == "outside"
            end
        end
    end

    @testset "unborn repository" begin
        with_repository(; commit = false) do
            write("new.jl", "x=1\n")
            GitRunic.git(["add", "new.jl"])

            @test GitRunic._main(["--staged", "--quiet"]) == 1
            @test GitRunic.git(["show", ":new.jl"]) == "x = 1"
            @test read("new.jl", String) == "x=1\n"
        end
    end

    @testset "temporary index isolation" begin
        with_repository() do
            write("base.jl", "base=2\n")
            GitRunic.git(["add", "base.jl"])
            gitdir = GitRunic.git(["rev-parse", "--git-dir"])
            sentinel = joinpath(gitdir, GitRunic.TEMP_INDEX_BASENAME)
            write(sentinel, "unrelated data")

            @test GitRunic._main(["--staged", "--quiet"]) == 1
            @test read(sentinel, String) == "unrelated data"
            @test isempty(
                filter(
                    name -> startswith(name, "$(GitRunic.TEMP_INDEX_BASENAME)-"),
                    readdir(gitdir),
                ),
            )
        end
    end

    @testset "rename and deletion" begin
        with_repository() do
            write("deleted.jl", "deleted = 1\n")
            write("old name.jl", "renamed = 1\n")
            GitRunic.git(["add", "."])
            GitRunic.git(["commit", "-qm", "add rename fixtures"])

            GitRunic.git(["mv", "old name.jl", "renamed λ.jl"])
            write("renamed λ.jl", "renamed=2\n")
            GitRunic.git(["add", "renamed λ.jl"])
            GitRunic.git(["rm", "-q", "deleted.jl"])

            @test GitRunic._main(["--staged", "--quiet"]) == 1
            @test GitRunic.git(["show", ":renamed λ.jl"]) == "renamed = 2"
            @test isempty(GitRunic.git(["ls-files", "--", "deleted.jl"]))
        end
    end

    @testset "unknown options" begin
        with_repository() do
            status = redirect_stderr(devnull) do
                GitRunic.main(["--definitely-not-an-option"])
            end
            @test status == 2

            status = redirect_stderr(devnull) do
                GitRunic.main(["--staged", "--patch"])
            end
            @test status == 2

            status = redirect_stderr(devnull) do
                GitRunic.main(["--commit=does-not-exist", "--quiet"])
            end
            @test status == 2
        end
    end

    @testset "parse errors are reported as CLI errors" begin
        with_repository() do
            write("base.jl", "base =\n")
            GitRunic.git(["add", "base.jl"])
            status, errmsg = mktemp() do _, err
                status = redirect_stderr(err) do
                    GitRunic.main(["--staged", "--quiet"])
                end
                flush(err)
                seekstart(err)
                return status, read(err, String)
            end
            @test status == 2
            @test occursin("failed to parse input from base.jl", errmsg)
        end
    end
end

end # module GitRunicTests
