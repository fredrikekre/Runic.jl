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
        else
            print(stderr, msg...)
        end
    end
    println(stderr)
    global errno = 1
    return errno
end

function main(argv)
    # Reset errno
    global errno = 0

    # Default values
    inputfiles = String[]
    outputfile = nothing
    verbose = false
    debug = false
    inplace = false

    # Parse the arguments
    while length(argv) > 0
        x = popfirst!(argv)
        if x == "-i"
            inplace = true
        elseif x == "-v"
            verbose = true
        elseif x == "-vv"
            debug = verbose = true
        elseif x == "-o"
            if length(argv) < 1
                return panic("expected output file as argument after `-o`")
            end
            outputfile = popfirst!(argv)
        else
            # Remaining arguments must be inputfile(s)
            push!(inputfiles, x)
            for x in argv
                if x == "-"
                    return panic("input \"-\" can not be used with multiple files")
                end
                push!(inputfiles, x)
            end
            break
        end
    end

    # stdin is the default input
    if isempty(inputfiles)
        push!(inputfiles, "-")
    end

    # multiple files require -i and no -o
    if length(inputfiles) > 1
        if !inplace
            return panic("option `-i` is required for multiple input files")
        elseif outputfile !== nothing
            return panic("option `-o` is incompatible with multiple input files")
        end
    end

    # inplace = true is incompatible with given output
    if inplace && outputfile !== nothing
        @assert length(inputfiles) == 1
        return panic("option `-i` is incompatible with option `-o $(outputfile)`")
    end

    # inplace = true is incompatible with stdin as input
    if inplace && first(inputfiles) == "-"
        return panic("option `-i` is incompatible with stdin as input")
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
        if inplace
            @assert outputfile === nothing
            @assert isfile(inputfile)
            @assert input_is_file
            # @assert length(inputfiles) == 1 # checked above
            output = inputfile
        else
            @assert length(inputfiles) == 1
            if outputfile === nothing || outputfile == "-"
                output = stdout
            elseif isfile(outputfile) && input_is_file && samefile(outputfile, inputfile)
                return panic("can not use same file for input and output, use `-i` to modify a file in place")
            else
                output = outputfile
            end
        end

        # Call the library to format the text
        ctx = try
            ctx = Context(sourcetext; verbose = verbose, debug = debug)
            format_tree!(ctx)
            ctx
        catch err
            panic(err)
            continue
        end

        # Write the output, but skip if inplace and it didn't change
        changed = ctx.fmt_tree !== ctx.src_tree
        if changed || !inplace
            try
                write(output, take!(ctx.fmt_io))
            catch err
                panic("could not write to output: ", err)
            end
        else
            # Log if verbose perhaps
        end

    end # inputfile loop

    return errno
end

@static if isdefined(Base, Symbol("@main"))
    @main
end
