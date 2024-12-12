# SPDX-License-Identifier: MIT

# Return code of main
errno::Cint = 0

# Check whether we are compiling with juliac
using Preferences: @load_preference
const juliac = @load_preference("juliac", false)

@static if juliac
    include("juliac.jl")
    const stdin = RawIO(RawFD(0))
    const stdout = RawIO(RawFD(1))
    const stderr = RawIO(RawFD(2))
    const run_cmd = run_juliac
    const printstyled = printstyled_juliac
    const mktempdir = mktempdir_juliac
    const sprint_showerror = sprint_showerror_juliac
    const print_vnum = print_vnum_juliac
else
    # const stdin = Base.stdin
    # const stdout = Base.stdout
    # const stderr = Base.stderr
    const run_cmd = Base.run
    const printstyled = Base.printstyled
    const mktempdir = Base.mktempdir
    sprint_showerror(err::Exception) = sprint(showerror, err)
    const print_vnum = Base.print
end

supports_color(io) = get(io, :color, false)

# juliac-compatible `Base.walkdir` but since we are collecting the files eagerly anyway we
# might as well use the same method even when not compiling with juliac.
function tryf(f::F, arg, default) where {F}
    try
        return f(arg)
    catch
        return default
    end
end
function scandir!(files, root)
    # Don't recurse into `.git`. If e.g. a branch name ends with `.jl` there are files
    # inside of `.git` which has the `.jl` extension, but they are not Julia source files.
    if occursin(".git", root) && ".git" in splitpath(root)
        @assert endswith(root, ".git")
        return
    end
    tryf(isdir, root, false) || return
    dirs = Vector{String}()
    for f in tryf(readdir, root, String[])
        jf = joinpath(root, f)
        if tryf(isdir, jf, false)
            push!(dirs, f)
        elseif (tryf(isfile, jf, false) || tryf(islink, jf, false)) && endswith(jf, ".jl")
            push!(files, jf)
        else
            # Ignore it I guess...
        end
    end
    for dir in dirs
        scandir!(files, joinpath(root, dir))
    end
    return
end

function panic(
        msg::String, err::Union{Exception, Nothing} = nothing,
        bt::Union{Vector{Base.StackFrame}, Nothing} = nothing
    )
    printstyled(stderr, "ERROR: "; color = :red, bold = true)
    print(stderr, msg)
    if err !== nothing
        print(stderr, sprint_showerror(err))
    end
    @static if juliac
        @assert bt === nothing
    else
        if bt !== nothing
            Base.show_backtrace(stderr, bt)
        end
    end
    println(stderr)
    global errno = 1
    return errno
end

blue(str) = printstyled(stderr, str; color = :blue)
function okln(str)
    blue(str)
    printstyled(stderr, "✔"; color = :green, bold = true)
    println(stderr)
    return
end
function errln(str)
    blue(str)
    printstyled(stderr, "✖"; color = :red, bold = true)
    println(stderr)
    return
end


# Print a typical cli program help message
function print_help()
    io = stdout
    printstyled(io, "NAME", bold = true)
    println(io)
    println(io, "       Runic.main - format Julia source code")
    println(io)
    printstyled(io, "SYNOPSIS", bold = true)
    println(io)
    println(io, "       julia -m Runic [<options>] <path>...")
    println(io)
    printstyled(io, "DESCRIPTION", bold = true)
    println(io)
    println(
        io, """
               `Runic.main` (typically invoked as `julia -m Runic`) formats Julia source
               code using the Runic.jl formatter.
        """
    )
    printstyled(io, "OPTIONS", bold = true)
    println(io)
    println(
        io, """
               <path>...
                   Input path(s) (files and/or directories) to process. For directories,
                   all files (recursively) with the '*.jl' suffix are used as input files.
                   If no path is given, or if path is `-`, input is read from stdin.

               -c, --check
                   Do not write output and exit with a non-zero code if the input is not
                   formatted correctly.

               -d, --diff
                   Print the diff between the input and formatted output to stderr.
                   Requires `git` to be installed.

               --help
                   Print this message.

               -i, --inplace
                   Format files in place.

               -o <file>, --output=<file>
                   File to write formatted output to. If no output is given, or if the file
                   is `-`, output is written to stdout.

               -v, --verbose
                   Enable verbose output.

               --version
                   Print Runic and julia version information.
        """
    )
    return
end

function print_version()
    print(stdout, "runic version ")
    print_vnum(stdout, RUNIC_VERSION)
    print(stdout, ", julia version ")
    print_vnum(stdout, VERSION)
    println(stdout)
    return
end

function maybe_expand_directory!(outfiles, dir)
    if !isdir(dir)
        # Assumed to be a file, checked when using it
        push!(outfiles, dir)
    else
        scandir!(outfiles, dir)
    end
    return
end

# juliac: type-stable output struct (required for juliac but useful in general too)
struct Output{IO}
    which::Symbol
    file::String
    stream::IO
    output_is_file::Bool
    output_is_samefile::Bool
end

function writeo(output::Output, iob)
    @assert output.which !== :devnull
    if output.which === :file
        # juliac: `open(...) do` uses dynamic dispatch
        # write(output.file, iob)
        let io = open(output.file, "w")
            try
                write(io, iob)
            finally
                close(io)
            end
        end
    elseif output.which == :stdout
        write(output.stream, iob)
    end
    return
end

function insert_line_range(line_ranges, lines)
    m = match(r"^(\d+):(\d+)$", lines)
    if m === nothing
        return panic("can not parse `--lines` argument as an integer range")
    end
    range_start = parse(Int, m.captures[1]::SubString)
    range_end = parse(Int, m.captures[2]::SubString)
    if range_start > range_end
        return panic("empty `--lines` range")
    end
    range = range_start:range_end
    if !all(x -> isdisjoint(x, range), line_ranges)
        return panic("`--lines` ranges cannot overlap")
    end
    push!(line_ranges, range)
    return 0
end

function main(argv)
    # Reset errno
    global errno = 0

    # Default values
    inputfiles = String[]
    outputfile = ""
    quiet = false
    verbose = false
    debug = false
    inplace = false
    diff = false
    check = false
    fail_fast = false
    line_ranges = typeof(1:2)[]

    # Parse the arguments
    while length(argv) > 0
        x = popfirst!(argv)
        if x == "-i" || x == "--inplace"
            inplace = true
        elseif x == "--help"
            print_help()
            return errno
        elseif x == "--version"
            print_version()
            return errno
        elseif x == "-q" || x == "--quiet"
            quiet = true
        elseif x == "-v" || x == "--verbose"
            verbose = true
        elseif x == "-v" || x == "--fail-fast"
            fail_fast = true
        elseif x == "-d" || x == "--diff"
            diff = true
        elseif x == "-c" || x == "--check"
            check = true
        elseif x == "-vv" || x == "--debug"
            debug = verbose = true
        elseif (m = match(r"^--lines=(.*)$", x); m !== nothing)
            if insert_line_range(line_ranges, m.captures[1]::SubString) != 0
                return errno
            end
        elseif x == "-o"
            if length(argv) < 1
                return panic("expected output file argument after `-o`")
            end
            outputfile = popfirst!(argv)
        elseif (m = match(r"^--output=(.+)$", x); m !== nothing)
            outputfile = String(m.captures[1]::SubString)
        else
            # Remaining arguments must be inputfile(s)
            maybe_expand_directory!(inputfiles, x)
            for x in argv
                if x == "-"
                    return panic("input `-` can not be used with multiple files")
                end
                maybe_expand_directory!(inputfiles, x)
            end
            break
        end
    end

    input_is_stdin = length(inputfiles) == 0 || inputfiles[1] == "-"

    # Check the arguments
    if inplace && check
        return panic("options `--inplace` and `--check` are mutually exclusive")
    end
    if inplace && outputfile != ""
        return panic("options `--inplace` and `--output` are mutually exclusive")
    end
    if check && outputfile != ""
        return panic("options `--check` and `--output` are mutually exclusive")
    end
    if inplace && input_is_stdin
        return panic("option `--inplace` can not be used together with stdin input")
    end
    if outputfile != "" && length(inputfiles) > 1
        return panic("option `--output` can not be used together with multiple input files")
    end
    if !isempty(line_ranges) && length(inputfiles) > 1
        return panic("option `--lines` can not be used together with multiple input files")
    end
    if length(inputfiles) > 1 && !(inplace || check)
        return panic("option `--inplace` or `--check` required with multiple input files")
    end

    if length(inputfiles) == 0
        push!(inputfiles, "-")
    end

    if diff
        if Sys.which("git") === nothing
            return panic("option `--diff` requires `git` to be installed")
        end
    end

    # Loop over the input files
    nfiles_str = string(length(inputfiles))
    for (file_counter, inputfile) in enumerate(inputfiles)
        # Read the input
        if input_is_stdin
            @assert length(inputfiles) == 1
            sourcetext = try
                read(stdin, String)
            catch err
                return panic("could not read input from stdin: ", err)
            end
        elseif isfile(inputfile)
            @assert !input_is_stdin
            sourcetext = try
                read(inputfile, String)
            catch err
                panic("could not read input from file `$(inputfile)`: ", err)
                continue
            end
        else
            panic("input file does not exist: `$(inputfile)`")
            continue
        end

        # Figure out output
        if inplace
            @assert outputfile == ""
            @assert isfile(inputfile)
            @assert !input_is_stdin
            output = Output(:file, inputfile, stdout, true, true)
        elseif check
            @assert outputfile == ""
            output = Output(:devnull, "", stdout, false, false)
        else
            @assert length(inputfiles) == 1
            if outputfile == "" || outputfile == "-"
                output = Output(:stdout, "", stdout, false, false)
            elseif isfile(outputfile) && !input_is_stdin && samefile(outputfile, inputfile)
                return panic("can not use same file for input and output, use `-i` to modify a file in place")
            else
                output = Output(:file, outputfile, stdout, true, false)
            end
        end

        # Print file info if `verbose` unless piping from/to stdin/stdout
        print_progress = verbose && !(input_is_stdin || !(output.output_is_file || check))

        # Print file info unless quiet and unless input/output is stdin/stdout
        if print_progress
            @assert inputfile != "-"
            input_pretty = relpath(inputfile)
            prefix = string(
                "[", lpad(string(file_counter), textwidth(nfiles_str), " "), "/",
                nfiles_str, "] "
            )
            if Sys.iswindows()
                input_pretty = replace(input_pretty, "\\" => "/")
            end
            if check
                str = string(prefix, "Checking `", input_pretty, "` ")
                ndots = 80 - textwidth(str) - 1 - 1
                dots = ndots > 0 ? "."^ndots : ""
                str = string(str, dots, " ")
            else
                if output.output_is_samefile
                    output_pretty = " "
                else
                    output_pretty = relpath(output.file)
                    if Sys.iswindows()
                        output_pretty = replace(output_pretty, "\\" => "/")
                    end
                    output_pretty = " -> `$(output_pretty)` "
                end
                str = string(prefix, "Formatting `", input_pretty, "`", output_pretty)
                ndots = 80 - textwidth(str) - 1 - 1
                dots = ndots > 0 ? "."^ndots : ""
                str = string(str, dots, " ")
            end
        end

        # Call the library to format the text
        ctx = try
            ctx′ = Context(sourcetext; quiet, verbose, debug, diff, check, line_ranges)
            format_tree!(ctx′)
            ctx′
        catch err
            print_progress && errln(str)
            if err isa JuliaSyntax.ParseError
                panic("failed to parse input: ", err)
                continue
            elseif err isa MainError
                panic(err.msg)
                continue
            end
            msg = "failed to format input: "
            @static if juliac
                rc = panic(msg, err)
            else
                # Limit stacktrace to 5 frames because Runic uses recursion a lot and 5
                # should be enough to see where the error occurred.
                bt = stacktrace(catch_backtrace())
                bt = bt[1:min(5, length(bt))]
                rc = panic(msg, err, bt)
            end
            if fail_fast
                return rc
            end
            continue
        end

        # Output the result
        changed = !nodes_equal(ctx.fmt_tree, ctx.src_tree)
        if check
            if changed
                print_progress && errln(str)
                global errno = 1
            else
                print_progress && okln(str)
            end
        elseif changed || !inplace
            @assert output.which !== :devnull
            try
                writeo(output, seekstart(ctx.fmt_io))
            catch err
                print_progress && errln(str)
                panic("could not write to output file `$(output.file)`: ", err)
                continue
            end
            print_progress && okln(str)
        else
            print_progress && okln(str)
        end
        if changed && diff
            mktempdir() do dir
                a = mkdir(joinpath(dir, "a"))
                b = mkdir(joinpath(dir, "b"))
                file = basename(inputfile)
                A = joinpath(a, file)
                B = joinpath(b, file)
                # juliac: `open(...) do` uses dynamic dispatch otherwise the following
                # blocks could be written as
                # ```
                # write(A, ctx.src_str)
                # write(B, seekstart(ctx.fmt_io))
                # ```
                let io = open(A, "w")
                    try
                        write(io, ctx.src_str)
                    finally
                        close(io)
                    end
                end
                let io = open(B, "w")
                    try
                        write(io, seekstart(ctx.fmt_io))
                    finally
                        close(io)
                    end
                end
                color = supports_color(stderr) ? "always" : "never"
                # juliac: Cmd string parsing uses dynamic dispatch
                # cmd = ```
                # $(git) --no-pager diff --color=$(color) --no-index --no-prefix
                #     $(relpath(A, dir)) $(relpath(B, dir))
                # ```
                git_argv = String[
                    Sys.which("git"), "--no-pager", "diff", "--color=$(color)", "--no-index", "--no-prefix",
                    relpath(A, dir), relpath(B, dir),
                ]
                cmd = Cmd(git_argv)
                # `ignorestatus` because --no-index implies --exit-code
                cmd = setenv(ignorestatus(cmd); dir = dir)
                cmd = pipeline(cmd, stdout = stderr, stderr = stderr)
                run_cmd(cmd)
                return
            end
        end

    end # inputfile loop

    return errno
end

@static if isdefined(Base, Symbol("@main"))
    @main
end
