# SPDX-License-Identifier: MIT

# juliac-compatible replacement for `read(stdin)`
function read_juliac()
    bytes = UInt8[]
    size = 1
    nmemb = 1024
    buf = zeros(UInt8, nmemb)
    file = Libc.FILE(RawFD(0), "r") # FILE constructor calls `fdopen`
    local fread, feof, ferror # Silence of the Langs(erver)
    while true
        nread = @ccall fread(buf::Ptr{UInt8}, size::Csize_t, nmemb::Cint, file::Ptr{Libc.FILE})::Csize_t
        append!(bytes, @view(buf[1:nread]))
        if nread < nmemb
            if (@ccall feof(file::Ptr{Libc.FILE})::Cint) != 0
                close(file)
                break
            else
                @assert (@ccall ferror(file::Ptr{Libc.FILE})::Cint) != 0
                close(file)
                error("ferror: fread failed")
            end
        end
    end
    return bytes
end

# juliac-compatible `Base.printstyled` that simply forces color
# TODO: detect color support (and maybe support `--color=(yes|no)`?), right now color is
# forced. For juliac we can detect whether stdout/stderr is a tty with
# `(@ccall isatty(RawFD(1)::Cint)::Cint) == 1`.
function printstyled_juliac(io::IO, str::String; bold = false, color::Symbol = :normal)
    @assert io === Core.stdout || io === Core.stderr
    @assert !occursin('\n', str)
    color === :red && write(io, "\e[31m")
    color === :green && write(io, "\e[32m")
    color === :blue && write(io, "\e[34m")
    color === :normal && write(io, "\e[0m")
    bold && write(io, "\e[1m")
    print(io, str)
    bold && write(io, "\e[22m")
    color in (:red, :green, :blue) && write(io, "\e[39m")
    return
end

# juliac-compatible `Base.showerror`
function sprint_showerror_juliac(err::Exception)
    if err isa SystemError
        return "SystemError: " * err.prefix * ": " * Libc.strerror(err.errnum)
    elseif err isa AssertionError
        # sprint uses dynamic dispatch
        io = IOBuffer()
        showerror(io, err)
        return String(take!(io))
    else
        return string(typeof(err))
    end
end

# juliac-compatible `Base.tempdir` and `Base.mktempdir` without logging and deferred cleanup
function tempdir_juliac()
    buf = Base.StringVector(Base.Filesystem.AVG_PATH - 1)
    sz = Base.RefValue{Csize_t}(length(buf) + 1)
    while true
        rc = ccall(:uv_os_tmpdir, Cint, (Ptr{UInt8}, Ptr{Csize_t}), buf, sz)
        if rc == 0
            resize!(buf, sz[])
            break
        elseif rc == Base.UV_ENOBUFS
            resize!(buf, sz[] - 1)
        else
            Base.uv_error("tempdir()", rc)
        end
    end
    tempdir = String(buf)
    return tempdir
end

function mktempdir_juliac()
    parent = tempdir_juliac()
    prefix = Base.Filesystem.temp_prefix
    if isempty(parent) || occursin(Base.Filesystem.path_separator_re, parent[end:end])
        tpath = "$(parent)$(prefix)XXXXXX"
    else
        tpath = "$(parent)$(Base.Filesystem.path_separator)$(prefix)XXXXXX"
    end
    req = Libc.malloc(Base._sizeof_uv_fs)
    try
        ret = ccall(
            :uv_fs_mkdtemp, Cint,
            (Ptr{Cvoid}, Ptr{Cvoid}, Cstring, Ptr{Cvoid}),
            C_NULL, req, tpath, C_NULL
        )
        if ret < 0
            Base.Filesystem.uv_fs_req_cleanup(req)
            Base.uv_error("mktempdir($(repr(parent)))", ret)
        end
        path = unsafe_string(ccall(:jl_uv_fs_t_path, Cstring, (Ptr{Cvoid},), req))
        Base.Filesystem.uv_fs_req_cleanup(req)
        return path
    finally
        Libc.free(req)
    end
end

function mktempdir_juliac(f::F) where {F}
    tmpdir = mktempdir_juliac()
    try
        f(tmpdir)
    finally
        try
            rm(tmpdir; force = true, recursive = true)
        catch
        end
    end
    return
end

# juliac-compatible `run(::Base.CmdRedirect)` where both stdout and stderr are redirected
# and read.
function run_juliac(cmd::Base.CmdRedirect)
    # Unpack the redirection layers
    @assert cmd.stream_no == 2
    @assert cmd.handle::Core.CoreSTDERR === Core.stderr
    cmd′ = cmd.cmd::Base.CmdRedirect
    @assert cmd′.stream_no == 1
    @assert cmd′.handle::Core.CoreSTDERR === Core.stderr
    cmd′′ = cmd′.cmd::Cmd
    @assert cmd′′.ignorestatus
    argv = cmd′′.exec
    dir = cmd′′.dir
    # Run the command
    bytes = pipe_fork_exec(argv, dir)
    # Write output
    write(Core.stderr, bytes)
    return
end

function WIFEXITED(status)
    return (status[] & 0x7f) == 0
end
function WEXITSTATUS(status)
    return (status[] & 0xff00) >> 8
end

function pipe_fork_exec(argv::Vector{String}, dir::String)
    local pipe, fork, dup2, chdir, execv, waitpid # Silence of the Langs(erver)
    # Set up the pipe
    fds = Vector{Cint}(undef, 2)
    READ_END, WRITE_END = 1, 2
    err = @ccall pipe(fds::Ref{Cint})::Cint
    err == -1 && systemerror("pipe")

    # Fork
    cpid = @ccall fork()::Cint
    cpid == -1 && systemerror("fork")

    # Handle the child process
    if cpid == 0
        # Close read end of the pipe
        err = @ccall close(fds[READ_END]::Cint)::Cint
        err == -1 && systemerror("close")
        # Duplicate write end of the pipe to stdout and stderr
        STDOUT_FILENO, STDERR_FILENO = 1, 2
        err = @ccall dup2(fds[WRITE_END]::Cint, STDOUT_FILENO::Cint)::Cint
        err == -1 && systemerror("dup2")
        err = @ccall dup2(fds[WRITE_END]::Cint, STDERR_FILENO::Cint)::Cint
        err = @ccall close(fds[WRITE_END]::Cint)::Cint # No longer needed
        err == -1 && systemerror("close")
        # Change directory
        err = @ccall chdir(dir::Cstring)::Cint
        err == 0 || systemerror("chdir")
        # Execute the command
        @ccall execv(argv[1]::Cstring, argv::Ref{Cstring})::Cint
        systemerror("execv")
    end

    # Continuing the parent process

    # Close write end of the pipe
    err = @ccall close(fds[WRITE_END]::Cint)::Cint
    err == -1 && systemerror("close")
    bytes = UInt8[]
    buf = Vector{UInt8}(undef, 1024)
    while true
        nread = @ccall read(fds[READ_END]::Cint, buf::Ptr{Cvoid}, 1024::Csize_t)::Cssize_t
        nread == -1 && systemerror("read")
        nread == 0 && break # eof
        append!(bytes, @view(buf[1:nread]))
    end
    err = @ccall close(fds[READ_END]::Cint)::Cint # Close the read end of the pipe
    err == -1 && systemerror("close")

    # Check exit status of the child
    status = Ref{Cint}()
    wpid = @ccall waitpid(cpid::Cint, status::Ref{Cint}, 0::Cint)::Cint
    wpid == -1 && systemerror("waitpid")
    if !WIFEXITED(status)
        error("child process did not exit normally")
    end
    # crc = WEXITSTATUS(status) # ignore this like `ignorestatus(cmd)`
    return bytes
end
