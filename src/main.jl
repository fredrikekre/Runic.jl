# SPDX-License-Identifier: MIT

@static if VERSION >= v"1.8"
    errno::Cint = 0
else
    errno = 0
end

function panic(msg...)
    printstyled(stderr, "ERROR: "; color = :red, bold = true)
    for m in msg
        if m isa Exception
            showerror(stderr, m)
        elseif m isa Vector{Base.StackFrame}
            Base.show_backtrace(stderr, m)
        else
            print(stderr, msg...)
        end
    end
    println(stderr)
    global errno = 1
    return errno
end

okln() = printstyled(stderr, "✔\n"; color = :green, bold = true)
errln() = printstyled(stderr, "✖\n"; color = :red, bold = true)

# Print a typical cli program help message
function print_help()
    io = stdout
    printstyled(io, "NAME\n", bold = true)
    println(io, "       Runic.main - format Julia source code")
    println(io)
    printstyled(io, "SYNOPSIS\n", bold = true)
    println(io, "       julia -m Runic [<options>] <path>...")
    println(io)
    printstyled(io, "DESCRIPTION\n", bold = true)
    println(io, """
                `Runic.main` (typically invoked as `julia -m Runic`) formats Julia source
                code using the Runic.jl formatter.
         """)
    printstyled(io, "OPTIONS\n", bold = true)
    println(io, """
                <path>...
                    Input path(s) (files and/or directories) to process. For directories,
                    all files (recursively) with the '*.jl' suffix are used as input files.
                    If path is `-` input is read from stdin.

                -c, --check
                    Do not write output and exit with a non-zero code if the input is not
                    formatted correctly.

                -d, --diff
                    Print the diff between the input and formatted output to stderr.
                    Requires `git` or `diff` to be installed.

                --fail-fast
                    Exit immediately after the first error. Only applicable when formatting
                    multiple files in the same invocation.

                --help
                    Print this message.

                -i, --inplace
                    Edit files in place. This option is required when passing multiple input
                    paths.

                -o, --output <file>
                    Output file to write formatted code to. If the specified file is `-`
                    output is written to stdout. This option can not be used together with
                    multiple input paths.
         """)
    return
end

function maybe_expand_directory!(outfiles, dir)
    if !isdir(dir)
        # Assumed a file, checked when using it
        push!(outfiles, dir)
        return
    end
    for (root, _, files) in walkdir(dir; onerror = (err) -> nothing)
        for file in files
            if endswith(file, ".jl")
                push!(outfiles, joinpath(root, file))
            end
        end
    end
end

function main(argv)
    # Reset errno
    global errno = 0

    # Split argv entries with `=`
    argv = mapreduce(x -> split(x, "="; limit = 2), append!, argv; init = String[])

    # Default values
    inputfiles = String[]
    outputfile = nothing
    quiet = false
    verbose = false
    debug = false
    inplace = false
    diff = false
    check = false
    fail_fast = false

    # Parse the arguments
    while length(argv) > 0
        x = popfirst!(argv)
        if x == "-i" || x == "--inplace"
            inplace = true
        elseif x == "--help"
            print_help()
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
        elseif x == "-o" || x == "--output"
            if length(argv) < 1
                return panic("expected output file as argument after `-o, --output`")
            end
            outputfile = popfirst!(argv)
        else
            # Remaining arguments must be inputfile(s)
            maybe_expand_directory!(inputfiles, x)
            for x in argv
                if x == "-"
                    return panic("input `-` can not be used with multiple files")
                else
                    maybe_expand_directory!(inputfiles, x)
                end
            end
            break
        end
    end

    # one of --check, --diff, --inplace, or --output must be specified
    if !(inplace || check || diff || outputfile !== nothing)
        return panic(
            "at least one of options `-c, --check`, `-d, --diff`, `-i, --inplace`, " *
                "or `-o, --output` must be specified",
        )
    end

    # --check can not be used with --inplace
    if inplace && check
        return panic("options `-c, --check` and `-i, --inplace` are mutually exclusive")
    end

    # stdin is the default input
    if isempty(inputfiles)
        return panic("no input files or directories specified")
    end

    # multiple files require --inplace or --check and no --output
    if length(inputfiles) > 1
        if !(inplace || check)
            return panic("option `-i, --inplace` or `-c, --check` is required for multiple input files")
        elseif outputfile !== nothing
            return panic("option `-o, --output` can not be used together with multiple input files")
        end
    end

    # --inplace can not be used when specifying output
    if inplace && outputfile !== nothing
        @assert length(inputfiles) == 1
        return panic("options `-i, --inplace` and `-o, --output` are  mutually exclusive")
    end

    # --inplace is incompatible with stdin as input
    if inplace && first(inputfiles) == "-"
        return panic("option `-i` is incompatible with stdin as input")
    end

    # --diff currently requires git
    git = nothing
    if diff
        git = Sys.which("git")
        if git === nothing
            return panic("option `-d, --diff` requires `git` to be installed")
        end
    end

    # Loop over the input files
    for inputfile in inputfiles
        # Read the input
        input_is_file = true
        if inputfile == "-"
            input_is_file = false
            sourcetext = try
                read(stdin, String)
            catch err
                return panic("could not read input from stdin: ", err)
            end
        elseif isfile(inputfile)
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

        # Check output
        output_is_file = false
        output_is_samefile = false
        if inplace
            @assert outputfile === nothing
            @assert isfile(inputfile)
            @assert input_is_file
            # @assert length(inputfiles) == 1 # checked above
            output = inputfile
            output_is_samefile = output_is_file = true
        elseif check
            @assert outputfile === nothing
            output = devnull
        else
            @assert length(inputfiles) == 1
            if outputfile === nothing
                return panic("no output file specified")
            elseif outputfile == "-"
                output = stdout
            elseif isfile(outputfile) && input_is_file && samefile(outputfile, inputfile)
                return panic("can not use same file for input and output, use `-i` to modify a file in place")
            else
                output = outputfile
                output_is_file = true
            end
        end

        # Print file info unless quiet and unless stdin and/or stdout is involved
        print_progress = !(quiet || !input_is_file || !(output_is_file || check))

        # Print file info unless quiet and unless formatting stdin -> stdout
        if print_progress
            input_pretty = relpath(inputfile)
            if check
                str = "Checking `$(input_pretty)` "
                ndots = 80 - textwidth(str) - 1 - 1
                dots = ndots > 0 ? "."^ndots : ""
                printstyled(stderr, str, dots, " "; color = :blue)
            else
                to = output_is_samefile ? " " : " -> `$(relpath(output))` "
                str = "Formatting `$(inputfile)`$(to)"
                ndots = 80 - textwidth(str) - 1 - 1
                dots = ndots > 0 ? "."^ndots : ""
                printstyled(stderr, str, dots, " "; color = :blue)
            end
        end

        # Call the library to format the text
        ctx = try
            ctx = Context(sourcetext; quiet, verbose, debug, diff, check)
            format_tree!(ctx)
            ctx
        catch err
            print_progress && errln()
            # Limit stacktrace to 5 frames because Runic uses recursion a lot and 5 should
            # be enough to see where the error occurred.
            bt = stacktrace(catch_backtrace())[1:5]
            rc = panic(err, bt)
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
            try
                write(output, seekstart(ctx.fmt_io))
            catch err
                print_progress && errln()
                panic("could not write to output: ", err)
            end
            print_progress && okln()
        else
            print_progress && okln()
        end
        if diff
            mktempdir() do dir
                a = mkdir(joinpath(dir, "a"))
                b = mkdir(joinpath(dir, "b"))
                file = basename(inputfile)
                A = joinpath(a, file)
                B = joinpath(b, file)
                write(A, ctx.src_str)
                write(B, seekstart(ctx.fmt_io))
                cmd = ```
                $(git) --no-pager diff --color=always --no-index --no-prefix
                    $(relpath(A, dir)) $(relpath(B, dir))
                ```
                # `ignorestatus` because --no-index implies --exit-code
                cmd = setenv(ignorestatus(cmd); dir = dir)
                cmd = pipeline(cmd, stdout = stderr, stderr = stderr)
                run(cmd)
            end
        end

    end # inputfile loop

    return errno
end

@static if isdefined(Base, Symbol("@main"))
    @main
end
