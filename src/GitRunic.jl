# SPDX-License-Identifier: MIT
#
# Based on `git-clang-format`, which is part of the LLVM Project, under the
# Apache License v2.0 with LLVM Exceptions (see https://llvm.org/LICENSE.txt).
# SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
#
# Translated from the Python implementation in bin/git-runic by Claude Code.

module GitRunic

import ..Context, ..format_tree!, ..format_markdown, ..is_markdown_file, ..MainError
import ..JuliaSyntax

const USAGE = "git runic [OPTIONS] [<commit>] [<commit>|--staged] [--] [<file>...]"

const DESCRIPTION = """
If zero or one commits are given, run runic on all lines that differ
between the working directory and <commit>, which defaults to HEAD.  Changes are
only applied to the working directory, or in the stage/index.

Examples:
  To format staged changes, i.e everything that's been `git add`ed:
    git runic

  To also format everything touched in the most recent commit:
    git runic HEAD~1

  If you're on a branch off main, to format everything touched on your branch:
    git runic main

If two commits are given (requires --diff), run runic on all lines in the
second <commit> that differ from the first <commit>.

The following git-config settings set the default of the corresponding option:
  runic.commit
  runic.extensions
"""

const TEMP_INDEX_BASENAME = "runic-index"

die(msg::AbstractString) = throw(MainError(msg))

# Run a git command, return stdout as a string. Throws MainError on failure.
function git(args::AbstractVector{<:AbstractString}; strip_output::Bool = true)
    cmd = Cmd(append!(String["git"], args))
    out = IOBuffer()
    err = IOBuffer()
    proc = run(pipeline(ignorestatus(cmd); stdout = out, stderr = err))
    stdout_str = String(take!(out))
    stderr_str = String(take!(err))
    if proc.exitcode == 0
        isempty(stderr_str) || print(stderr, stderr_str)
        return strip_output ? rstrip(stdout_str, ('\r', '\n')) : stdout_str
    end
    isempty(stderr_str) || print(stderr, rstrip(stderr_str, ('\r', '\n')), "\n")
    return die("`git $(join(args, " "))` returned $(proc.exitcode)")
end

# Load git configuration as a Dict{String,String}.
function load_git_config()
    config = Dict{String, String}()
    out = IOBuffer()
    proc = run(
        pipeline(
            ignorestatus(Cmd(["git", "config", "--list", "--null"]));
            stdout = out, stderr = devnull
        )
    )
    proc.exitcode != 0 && return config
    for entry in split(String(take!(out)), '\0')
        isempty(entry) && continue
        nl = findfirst('\n', entry)
        if nl !== nothing
            config[entry[1:(nl - 1)]] = entry[(nl + 1):end]
        else
            # A setting with no value is implicitly 'true'
            config[entry] = "true"
        end
    end
    return config
end

# Return the git object type for value, or nothing if not a valid object.
function get_object_type(value::AbstractString)
    out = IOBuffer()
    proc = run(
        pipeline(
            ignorestatus(Cmd(["git", "cat-file", "-t", value]));
            stdout = out, stderr = devnull
        )
    )
    proc.exitcode != 0 && return nothing
    return strip(String(take!(out)))
end

# Return true if value is a revision, false if it is a file path, or die.
function disambiguate_revision(value::AbstractString)
    proc = run(
        pipeline(
            ignorestatus(Cmd(["git", "rev-parse", value]));
            stdout = devnull, stderr = devnull
        )
    )
    proc.exitcode != 0 && die("'$value' is neither a commit nor a file path")
    object_type = get_object_type(value)
    object_type === nothing && return false
    object_type in ("commit", "tag") && return true
    return die("`$value` is a $object_type, but a commit or filename was expected")
end

# Interpret positional args as "[commits] [--] [files]", return (commits, files).
function interpret_args(args::Vector{String}, dash_dash::Vector{String}, default_commit::String)
    if !isempty(dash_dash)
        commits = isempty(args) ? [default_commit] : copy(args)
        for commit in commits
            object_type = get_object_type(commit)
            if !(object_type in ("commit", "tag"))
                if object_type === nothing
                    die("'$commit' is not a commit")
                else
                    die("'$commit' is a $object_type, but a commit was expected")
                end
            end
        end
        files = dash_dash[2:end]  # skip "--" itself
    elseif !isempty(args)
        args = copy(args)
        commits = String[]
        while !isempty(args)
            disambiguate_revision(args[1]) || break
            push!(commits, popfirst!(args))
        end
        isempty(commits) && (commits = [default_commit])
        files = args
    else
        commits = [default_commit]
        files = String[]
    end
    return commits, files
end

# Run git diff and return (output_string, exitcode).
function compute_diff(commits, files, staged::Bool, diff_common_commit::Bool)
    git_tool = "diff-index"
    extra_args = String[]
    commits_arg = copy(commits)
    if length(commits) == 2
        git_tool = "diff-tree"
        if diff_common_commit
            commits_arg = ["$(commits[1])...$(commits[2])"]
        end
    elseif staged
        push!(extra_args, "--cached")
    end
    # `HEAD` (or any other spelling of it, e.g. `@`) does not resolve in a repository
    # without commits. Treat an unresolvable commit in an unborn repository as the
    # empty tree so that staged additions in a new repository can still be formatted.
    if length(commits_arg) == 1 && get_object_type("HEAD") === nothing &&
            get_object_type(commits_arg[1]) === nothing
        out = IOBuffer()
        proc = run(
            pipeline(
                ignorestatus(Cmd(["git", "mktree"]));
                stdin = devnull, stdout = out, stderr = stderr
            )
        )
        proc.exitcode == 0 || die("`git mktree` returned $(proc.exitcode)")
        commits_arg = [rstrip(String(take!(out)), ('\r', '\n'))]
    end
    cmd_args = String["git", git_tool, "-p", "-U0"]
    append!(cmd_args, extra_args)
    append!(cmd_args, commits_arg)
    push!(cmd_args, "--")
    append!(cmd_args, files)
    out = IOBuffer()
    proc = run(pipeline(ignorestatus(Cmd(cmd_args)); stdout = out, stderr = stderr))
    return String(take!(out)), proc.exitcode
end

# Compute diff and return Dict mapping filename to changed line ranges.
function compute_diff_and_extract_lines(commits, files, staged::Bool, diff_common_commit::Bool)
    output, exitcode = compute_diff(commits, files, staged, diff_common_commit)
    exitcode != 0 && die("git diff failed")
    return extract_lines(output)
end

# Decode the filename from a `+++` diff header. Git C-quotes paths containing
# non-ASCII bytes or control characters when `core.quotePath` is enabled.
function extract_filename(line::AbstractString)
    startswith(line, "+++ ") || return nothing
    path = String(SubString(line, 5))
    if startswith(path, '"')
        m = match(r"^\"(.*)\"(?:\t.*)?$", path)
        m === nothing && return nothing
        path = Base.unescape_string(String(m.captures[1]::SubString))
    else
        path = rstrip(path, ('\r', '\n', '\t'))
    end
    path == "/dev/null" && return nothing
    startswith(path, "b/") || return nothing
    return String(SubString(path, 3))
end

# Parse a -U0 unified diff, returning Dict{filename => Vector{UnitRange{Int}}}.
function extract_lines(patch::AbstractString)
    changed = Dict{String, Vector{UnitRange{Int}}}()
    filename = nothing
    in_hunk = false
    for line in eachline(IOBuffer(patch))
        if startswith(line, "diff --git ")
            filename = nothing
            in_hunk = false
        elseif !in_hunk && startswith(line, "+++ ")
            filename = extract_filename(line)
        end
        m = match(r"^@@ -[0-9,]+ \+(\d+)(,(\d+))?", line)
        if m !== nothing && filename !== nothing
            in_hunk = true
            start_line = parse(Int, m.captures[1]::SubString)
            line_count = m.captures[3] !== nothing ? parse(Int, m.captures[3]::SubString) : 1
            line_count == 0 && (line_count = 1)
            start_line == 0 && continue
            push!(
                get!(Vector{UnitRange{Int}}, changed, filename),
                start_line:(start_line + line_count - 1)
            )
        end
    end
    return changed
end

# Remove entries from dict whose extension is not in allowed_extensions.
function filter_by_extension!(dict, allowed_extensions)
    allowed = Set{String}(allowed_extensions)
    for filename in collect(keys(dict))
        parts = rsplit(filename, '.'; limit = 2)
        if length(parts) == 1
            "" ∉ allowed && delete!(dict, filename)
        else
            lowercase(parts[2]) ∉ allowed && delete!(dict, filename)
        end
    end
    return
end

# Prefix pathspecs with the literal magic so that filenames containing glob
# metacharacters (`*`, `?`, `[`, ...) match exactly instead of as patterns.
literal_pathspecs(filenames) = String[":(literal)" * f for f in filenames]

# Remove symlink entries from dict. `revision` is nothing for the worktree,
# "" for the index, or a commit/tree containing the files being formatted.
function filter_symlinks!(dict, revision = nothing)
    if revision === nothing
        for filename in collect(keys(dict))
            islink(filename) && delete!(dict, filename)
        end
        return
    end
    isempty(dict) && return
    # Single batched call for all files. `-z` so that paths round-trip unquoted.
    # `ls-files --stage` entries are `<mode> <sha> <stage>\t<path>` and `ls-tree -r`
    # entries are `<mode> <type> <sha>\t<path>`: the mode is the first field and the
    # path follows the tab in both.
    pathspecs = literal_pathspecs(keys(dict))
    output = if isempty(revision)
        git(["ls-files", "--stage", "-z", "--", pathspecs...]; strip_output = false)
    else
        git(["ls-tree", "-r", "-z", revision, "--", pathspecs...]; strip_output = false)
    end
    for entry in split(rstrip(output, '\0'), '\0')
        isempty(entry) && continue
        meta, filename = split(entry, '\t'; limit = 2)
        mode = first(split(meta))
        mode == "120000" && delete!(dict, filename)
    end
    return
end

# Change the working directory to the git repository root.
function cd_to_toplevel()
    return cd(git(["rev-parse", "--show-toplevel"]))
end

# Format a file mode integer as a zero-prefixed octal string (git index format).
function format_mode(mode_int::Integer)
    return "0" * string(Int(mode_int); base = 8)
end

# Create a git tree object from files in the working directory.
function create_tree_from_workdir(filenames)
    return create_tree(filenames, "--stdin")
end

# Create a git tree object from files in the index.
function create_tree_from_index(filenames)
    lines = SubString{String}[]
    if !isempty(filenames)
        output = git(
            ["ls-files", "--stage", "-z", "--", literal_pathspecs(filenames)...];
            strip_output = false
        )
        append!(lines, filter(!isempty, split(rstrip(output, '\0'), '\0')))
    end
    return create_tree(lines, "--index-info")
end

# Map filename => entry mode (as a string) for the given files. `revision` is nothing
# for the worktree, "" for the index, or a commit/tree containing the files.
function file_modes(filenames, revision)
    modes = Dict{String, String}()
    if revision === nothing
        for filename in filenames
            modes[filename] = format_mode(stat(filename).mode)
        end
        return modes
    end
    isempty(filenames) && return modes
    output = if isempty(revision)
        git(
            ["ls-files", "--stage", "-z", "--", literal_pathspecs(filenames)...];
            strip_output = false
        )
    else
        git(
            ["ls-tree", "-r", "-z", revision, "--", literal_pathspecs(filenames)...];
            strip_output = false
        )
    end
    for entry in split(rstrip(output, '\0'), '\0')
        isempty(entry) && continue
        meta, filename = split(entry, '\t'; limit = 2)
        modes[filename] = format_mode(parse(Int, first(split(meta)); base = 8))
    end
    return modes
end

# Format a file using Runic in-process and store the result as a git blob.
# revision is nothing (workdir), "" (index/staged), or a commit SHA.
# Returns the blob SHA.
function runic_to_blob(filename, line_ranges::Vector{UnitRange{Int}}; revision = nothing)
    # Get source content from the working directory or a git object
    if revision !== nothing
        show_args = String["git", "cat-file", "blob", "$(revision):$(filename)"]
        out = IOBuffer()
        proc = run(pipeline(ignorestatus(Cmd(show_args)); stdout = out, stderr = stderr))
        proc.exitcode != 0 && die("`$(join(show_args, " "))` failed")
        src_str = String(take!(out))
    else
        src_str = read(filename, String)
    end

    # Format in-process. Dispatch on extension: `.md` and `.qmd` through the Markdown
    # formatter, everything else through the Julia formatter. Mirrors the
    # extension dispatch in `Runic.main` / `Runic.format_file`.
    local fmt_io::IO
    try
        if is_markdown_file(filename)
            fmt_io = IOBuffer(format_markdown(src_str; line_ranges = line_ranges))
        else
            ctx = Context(src_str; line_ranges = line_ranges, filename = filename)
            format_tree!(ctx)
            fmt_io = seekstart(ctx.fmt_io)
        end
    catch err
        err isa JuliaSyntax.ParseError || rethrow()
        die("failed to parse input from $filename: $(sprint(showerror, err))")
    end

    # Store formatted output as a git blob and return its SHA
    hash_args = String["git", "hash-object", "-w", "--path=$(filename)", "--stdin"]
    out = IOBuffer()
    proc = run(
        pipeline(
            ignorestatus(Cmd(hash_args));
            stdin = fmt_io, stdout = out, stderr = stderr
        )
    )
    proc.exitcode != 0 && die("`$(join(hash_args, " "))` failed")
    return rstrip(String(take!(out)), ('\r', '\n'))
end

# Format all changed files in-process and save results to a new git tree.
function run_runic_and_save_to_tree(changed_lines, revision)
    index_info_lines = String[]
    modes = file_modes(keys(changed_lines), revision)
    for (filename, line_ranges) in changed_lines
        mode = modes[filename]
        blob_id = runic_to_blob(filename, line_ranges; revision = revision)
        push!(index_info_lines, "$(mode) $(blob_id)\t$(filename)")
    end
    return create_tree(index_info_lines, "--index-info")
end

# Set GIT_INDEX_FILE to a temporary index, call f(), then restore and delete the index.
# If tree is given, seed the index from that tree; otherwise start with an empty index.
function with_temporary_index(f, tree = nothing)
    gitdir = abspath(git(["rev-parse", "--git-dir"]))
    tempdir = mktempdir(gitdir; prefix = "$(TEMP_INDEX_BASENAME)-")
    path = joinpath(tempdir, "index")
    return try
        if tree === nothing
            git(["read-tree", "--index-output=$(path)", "--empty"])
        else
            git(["read-tree", "--index-output=$(path)", tree])
        end
        old_index = get(ENV, "GIT_INDEX_FILE", nothing)
        ENV["GIT_INDEX_FILE"] = path
        try
            return f()
        finally
            if old_index === nothing
                delete!(ENV, "GIT_INDEX_FILE")
            else
                ENV["GIT_INDEX_FILE"] = old_index
            end
        end
    finally
        rm(tempdir; force = true, recursive = true)
    end
end

# Create a git tree object from input_lines using a temporary index.
# mode is "--stdin" (filenames) or "--index-info" ("<mode> <sha>\t<path>").
function create_tree(input_lines, mode::String)
    @assert mode in ("--stdin", "--index-info")
    cmd = Cmd(["git", "update-index", "--add", "-z", mode])
    return with_temporary_index() do
        inp = IOBuffer()
        for line in input_lines
            write(inp, string(line), '\0')
        end
        proc = run(
            pipeline(
                ignorestatus(cmd);
                stdin = seekstart(inp), stdout = devnull,
                stderr = stderr
            )
        )
        proc.exitcode != 0 && die("`git update-index --add -z $(mode)` failed")
        git(["write-tree"])
    end
end

# Print diff between two git trees to stdout.
function print_diff(old_tree, new_tree)
    cmd = Cmd(["git", "diff", "--diff-filter=M", "--exit-code", old_tree, new_tree])
    return run(pipeline(ignorestatus(cmd); stdout = stdout, stderr = stderr)).exitcode
end

# Print diffstat between two git trees to stdout.
function print_diffstat(old_tree, new_tree)
    cmd = Cmd(["git", "diff", "--diff-filter=M", "--exit-code", "--stat", old_tree, new_tree])
    return run(pipeline(ignorestatus(cmd); stdout = stdout, stderr = stderr)).exitcode
end

# Return the files that differ between the two trees (i.e. the files the formatter
# modified).
function modified_files(old_tree, new_tree)
    result = git(
        [
            "diff-tree", "--diff-filter=M", "-r", "-z", "--name-only",
            old_tree, new_tree,
        ]; strip_output = false
    )
    return filter(!isempty, split(rstrip(result, '\0'), '\0'))
end

# Apply changes from new_tree to the working directory.
function apply_changes(old_tree, new_tree; force::Bool = false, patch_mode::Bool = false)
    changed_files = modified_files(old_tree, new_tree)

    if !force
        unstaged = git(
            ["diff-files", "--name-status", "--", literal_pathspecs(changed_files)...]
        )
        if !isempty(unstaged)
            die(
                "the following files would be modified but have unstaged changes:\n" *
                    unstaged * "\nPlease commit, stage, or stash them first."
            )
        end
    end

    if patch_mode
        with_temporary_index(old_tree) do
            run(
                pipeline(
                    ignorestatus(Cmd(["git", "checkout", "--patch", new_tree]));
                    stdout = stdout, stderr = stderr
                )
            )
        end
    else
        with_temporary_index(new_tree) do
            git(["checkout-index", "-f", "--", changed_files...])
        end
    end

    return changed_files
end

# Apply formatted blobs to the real index without touching the working tree.
function apply_changes_to_index(old_tree, new_tree)
    changed_files = modified_files(old_tree, new_tree)
    isempty(changed_files) && return changed_files

    # Limit the index update to the files the formatter modified so that entries for
    # unmodified files keep their flags (e.g. skip-worktree) and stat cache.
    index_info = git(
        [
            "ls-tree", "-r", "-z", new_tree, "--",
            literal_pathspecs(changed_files)...,
        ]; strip_output = false
    )
    proc = run(
        pipeline(
            ignorestatus(Cmd(["git", "update-index", "-z", "--index-info"]));
            stdin = IOBuffer(index_info), stdout = devnull, stderr = stderr
        )
    )
    proc.exitcode == 0 || die("`git update-index -z --index-info` failed")
    return changed_files
end

function print_help()
    io = stdout
    println(io, "usage: $(USAGE)")
    println(io)
    print(io, DESCRIPTION)
    println(io, "options:")
    println(io, "  --commit COMMIT       default commit if none is specified (default: HEAD)")
    println(io, "  --diff                print a diff instead of applying the changes")
    println(io, "  --diffstat            print a diffstat instead of applying the changes")
    println(io, "  --extensions LIST     comma-separated list of file extensions (default: jl)")
    println(io, "  -f, --force           allow changes to unstaged files")
    println(io, "  -p, --patch           select hunks interactively")
    println(io, "  -q, --quiet           print less information")
    println(io, "  --staged, --cached    format lines in the stage instead of the working dir")
    println(io, "  -v, --verbose         print extra information")
    println(io, "  --diff-from-common-commit")
    return println(io, "                        diff from last common commit (requires two commits)")
end

function main(argv)
    try
        return _main(argv)
    catch e
        e isa MainError || rethrow()
        printstyled(stderr, "ERROR: "; color = :red, bold = true)
        println(stderr, e.msg)
        return 2
    end
end

function _main(argv)
    config = load_git_config()

    # Split off everything at and after "--"
    dash_idx = findfirst(==("--"), argv)
    if dash_idx !== nothing
        dash_dash = argv[dash_idx:end]
        argv = argv[1:(dash_idx - 1)]
    else
        dash_dash = String[]
    end

    # Defaults, overridden by git config or command-line flags
    default_commit = get(config, "runic.commit", "HEAD")
    extensions = get(config, "runic.extensions", "jl")

    diff_mode = false
    diffstat_mode = false
    force = false
    patch_mode = false
    verbose = 0
    staged = false
    diff_from_common_commit = false
    positional = String[]

    i = 1
    while i <= length(argv)
        arg = argv[i]
        if arg == "--help" || arg == "-h"
            print_help()
            return 0
        elseif arg == "--diff"
            diff_mode = true
        elseif arg == "--diffstat"
            diffstat_mode = true
        elseif arg == "-f" || arg == "--force"
            force = true
        elseif arg == "-p" || arg == "--patch"
            patch_mode = true
        elseif arg == "-q" || arg == "--quiet"
            verbose -= 1
        elseif arg == "-v" || arg == "--verbose"
            verbose += 1
        elseif arg == "--staged" || arg == "--cached"
            staged = true
        elseif arg == "--diff-from-common-commit" || arg == "--diff_from_common_commit"
            diff_from_common_commit = true
        elseif (m = match(r"^--commit=(.+)$", arg); m !== nothing)
            default_commit = String(m.captures[1]::SubString)
        elseif arg == "--commit"
            i += 1; i > length(argv) && die("expected argument after --commit")
            default_commit = argv[i]
        elseif (m = match(r"^--extensions=(.+)$", arg); m !== nothing)
            extensions = String(m.captures[1]::SubString)
        elseif arg == "--extensions"
            i += 1; i > length(argv) && die("expected argument after --extensions")
            extensions = argv[i]
        elseif startswith(arg, '-')
            die("unknown option: $arg")
        else
            push!(positional, arg)
        end
        i += 1
    end

    commits, files = interpret_args(positional, dash_dash, default_commit)

    if length(commits) > 2
        die("at most two commits allowed; $(length(commits)) given")
    end
    staged && patch_mode && die("--patch is not allowed with --staged")
    if length(commits) == 2
        staged && die("--staged is not allowed when two commits are given")
        diff_mode || die("--diff is required when two commits are given")
    elseif diff_from_common_commit
        die("--diff-from-common-commit is only allowed when two commits are given")
    end

    changed_lines = compute_diff_and_extract_lines(
        commits, files, staged,
        diff_from_common_commit
    )

    ignored_files = verbose >= 1 ? Set{String}(keys(changed_lines)) : Set{String}()

    filter_by_extension!(changed_lines, split(lowercase(extensions), ','))
    cd_to_toplevel()
    source_revision = length(commits) > 1 ? commits[2] : staged ? "" : nothing
    filter_symlinks!(changed_lines, source_revision)

    if verbose >= 1
        setdiff!(ignored_files, keys(changed_lines))
        if !isempty(ignored_files)
            println(
                "Ignoring the following files (wrong extension, symlink, or " *
                    "ignored by runic):"
            )
            for f in sort!(collect(ignored_files))
                println("    $f")
            end
        end
        if !isempty(changed_lines)
            println("Running runic on the following files:")
            for f in sort!(collect(keys(changed_lines)))
                println("    $f")
            end
        end
    end

    if isempty(changed_lines)
        verbose >= 0 && println("no modified files to format")
        return 0
    end

    if length(commits) > 1
        old_tree = commits[2]
        revision = old_tree
    elseif staged
        old_tree = create_tree_from_index(keys(changed_lines))
        revision = ""
    else
        old_tree = create_tree_from_workdir(keys(changed_lines))
        revision = nothing
    end

    new_tree = run_runic_and_save_to_tree(changed_lines, revision)

    if verbose >= 1
        println("old tree: $old_tree")
        println("new tree: $new_tree")
    end

    if old_tree == new_tree
        verbose >= 0 && println("runic did not modify any files")
        return 0
    end

    diff_mode && return print_diff(old_tree, new_tree)
    diffstat_mode && return print_diffstat(old_tree, new_tree)

    changed_files = if staged
        apply_changes_to_index(old_tree, new_tree)
    else
        apply_changes(old_tree, new_tree; force = force, patch_mode = patch_mode)
    end
    if (verbose >= 0 && !patch_mode) || verbose >= 1
        println("changed files:")
        for f in changed_files
            println("    $f")
        end
    end

    return 1
end

@static if isdefined(Base, Symbol("@main"))
    @main
end

end # module GitRunic
