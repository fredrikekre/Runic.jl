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

function okln()
    printstyled(stderr, "✔"; color = :green, bold = true)
    println(stderr)
    return
end
function errln()
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

               --lines=<start line>:<end line>
                   Limit formatting to the line range <start line> to <end line>. Multiple
                   ranges can be formatted by specifying multiple --lines arguments.

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
    input_is_stdin = true
    multiple_inputs = false

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
            # Remaining arguments must be `-`, files, or directories
            first = true
            while true
                if x == "-"
                    # `-` is only allowed once and only in first position
                    if length(argv) > 0 || !first
                        return panic("input `-` can not be combined with other input")
                    end
                    push!(inputfiles, x)
                    input_is_stdin = true
                else
                    input_is_stdin = false
                    if isdir(x)
                        scandir!(inputfiles, x)
                        # Directories are considered to be multiple (potential) inputs even
                        # if they end up being empty
                        multiple_inputs = true
                    else # isfile(x)
                        push!(inputfiles, x) # Assume it is a file for now
                    end
                end
                length(argv) == 0 && break
                x = popfirst!(argv)
                first = false
                multiple_inputs = true
            end
            break
        end
    end

    # Insert `-` as the input if there were no input files/directories on the command line
    if input_is_stdin && length(inputfiles) == 0
        @assert !multiple_inputs
        push!(inputfiles, "-")
    end

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
    if outputfile != "" && multiple_inputs
        # TODO: Why not?
        return panic("option `--output` can not be used together with multiple input files")
    end
    if !isempty(line_ranges) && multiple_inputs
        return panic("option `--lines` can not be used together with multiple input files")
    end
    if multiple_inputs && !(inplace || check)
        return panic("option `--inplace` or `--check` required with multiple input files")
    end

    if diff
        if Sys.which("git") === nothing
            return panic("option `--diff` requires `git` to be installed")
        end
    end

    # Disable verbose if piping from/to stdin/stdout
    output_is_stdout = !inplace && !check && (outputfile == "" || outputfile == "-")
    print_progress = verbose && !(input_is_stdin || output_is_stdout)

    # Loop over the input files
    nfiles_str = string(length(inputfiles))
    for (file_counter, inputfile) in enumerate(inputfiles)

        if print_progress
            @assert inputfile != "-"
            input_pretty = relpath(inputfile)
            if Sys.iswindows()
                input_pretty = replace(input_pretty, "\\" => "/")
            end
            prefix = string(
                "[", lpad(string(file_counter), textwidth(nfiles_str), " "), "/",
                nfiles_str, "] "
            )
            verb = check ? "Checking" : "Formatting"
            str = string(prefix, verb, " `", input_pretty, "` ")
            ndots = 80 - textwidth(str) - 1 - 1
            dots = ndots > 0 ? "."^ndots : ""
            printstyled(stderr, string(str, dots, " "); color = :blue)
        end

        # Read the input
        if inputfile == "-"
            @assert input_is_stdin
            @assert length(inputfiles) == 1
            @assert !multiple_inputs
            sourcetext = try
                read(stdin, String)
            catch err
                print_progress && errln()
                panic("could not read input from stdin: ", err)
                continue
            end
        elseif isfile(inputfile)
            @assert !input_is_stdin
            sourcetext = try
                read(inputfile, String)
            catch err
                print_progress && errln()
                panic("could not read input from file `$(inputfile)`: ", err)
                continue
            end
        else
            print_progress && errln()
            panic("input path is not a file or directory: `$(inputfile)`")
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
            @assert !multiple_inputs
            if outputfile == "" || outputfile == "-"
                output = Output(:stdout, "", stdout, false, false)
            elseif isfile(outputfile) && !input_is_stdin && samefile(outputfile, inputfile)
                print_progress && errln()
                panic("can not use same file for input and output, use `-i` to modify a file in place")
                continue
            else
                output = Output(:file, outputfile, stdout, true, false)
            end
        end

        # Call the library to format the text
        inputfile_pretty = inputfile == "-" ? "stdin" : inputfile
        ctx = try
            ctx′ = Context(
                sourcetext; quiet, verbose, debug, diff, check, line_ranges,
                filename = inputfile_pretty,
            )
            format_tree!(ctx′)
            ctx′
        catch err
            print_progress && errln()
            if err isa JuliaSyntax.ParseError
                panic(string("failed to parse input from ", inputfile_pretty, ": "), err)
                continue
            elseif err isa MainError
                panic(err.msg)
                continue
            end
            msg = string("failed to format input from ", inputfile_pretty, ": ")
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
                print_progress && errln()
                global errno = 1
            else
                print_progress && okln()
            end
        elseif changed || !inplace
            @assert output.which !== :devnull
            try
                writeo(output, seekstart(ctx.fmt_io))
            catch err
                print_progress && errln()
                panic("could not write to output file `$(output.file)`: ", err)
                continue
            end
            print_progress && okln()
        else
            print_progress && okln()
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
