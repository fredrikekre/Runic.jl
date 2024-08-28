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
    println(
        io, """
               `Runic.main` (typically invoked as `julia -m Runic`) formats Julia source
               code using the Runic.jl formatter.
        """
    )
    printstyled(io, "OPTIONS\n", bold = true)
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
        """
    )
    return
end

function maybe_expand_directory!(outfiles, dir)
    if !isdir(dir)
        # Assumed a file, checked when using it
        push!(outfiles, dir)
        return
    end
    for (root, _, files) in walkdir(dir; onerror = (err) -> nothing)
        # Don't recurse into `.git`. If e.g. a branch name ends with `.jl` there are files
        # inside of `.git` which has the `.jl` extension, but they are not Julia source
        # files.
        if occursin(".git", root) && ".git" in splitpath(root)
            continue
        end
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
    if inplace && outputfile !== nothing
        return panic("options `--inplace` and `--output` are mutually exclusive")
    end
    if check && outputfile !== nothing
        return panic("options `--check` and `--output` are mutually exclusive")
    end
    if inplace && input_is_stdin
        return panic("option `--inplace` can not be used together with stdin input")
    end
    if outputfile !== nothing && length(inputfiles) > 1
        return panic("option `--output` can not be used together with multiple input files")
    end
    if length(inputfiles) > 1 && !(inplace || check)
        return panic("option `--inplace` or `--check` required with multiple input files")
    end

    if length(inputfiles) == 0
        push!(inputfiles, "-")
    end

    git = ""
    if diff
        git = something(Sys.which("git"), git)
        if isempty(git)
            return panic("option `--diff` requires `git` to be installed")
        end
    end

    # Loop over the input files
    for inputfile in inputfiles
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
        output_is_file = false
        output_is_samefile = false
        if inplace
            @assert outputfile === nothing
            @assert isfile(inputfile)
            @assert !input_is_stdin
            output = inputfile
            output_is_samefile = output_is_file = true
        elseif check
            @assert outputfile === nothing
            output = devnull
        else
            @assert length(inputfiles) == 1
            if outputfile === nothing || outputfile == "-"
                output = stdout
            elseif isfile(outputfile) && !input_is_stdin && samefile(outputfile, inputfile)
                return panic("can not use same file for input and output, use `-i` to modify a file in place")
            else
                output = outputfile
                output_is_file = true
            end
        end

        # Print file info unless quiet and unless stdin and/or stdout is involved
        print_progress = !(quiet || input_is_stdin || !(output_is_file || check))

        # Print file info unless quiet and unless input/output is stdin/stdout
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
            ctx′ = Context(sourcetext; quiet, verbose, debug, diff, check)
            format_tree!(ctx′)
            ctx′
        catch err
            print_progress && errln()
            # Limit stacktrace to 5 frames because Runic uses recursion a lot and 5 should
            # be enough to see where the error occurred.
            bt = stacktrace(catch_backtrace())
            bt = bt[1:min(5, length(bt))]
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
            @assert output !== devnull
            try
                write(output, seekstart(ctx.fmt_io))
            catch err
                print_progress && errln()
                panic("could not write to output file `$(output)`: ", err)
            end
            print_progress && okln()
        else
            print_progress && okln()
        end
        if diff
            @assert git !== ""
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
