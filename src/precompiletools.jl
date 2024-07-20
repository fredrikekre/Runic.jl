using PrecompileTools: @setup_workload, @compile_workload
@setup_workload begin
    bad_bad_code = """# Some ill-formatted julia code to fire-up Runic.jl!
    # This is simply taken from runstone
    print( "chaotic use of spaces (yes there's one space at the end of the line") 
      
    \tprint( "chaotic use of tabs")\t 
    a = 0x1
    b= 1.
    c=1.00
    d=0+2    


    e=(
    1,2,3,
    4)
    ( e...,)
    a==3 ? println(   "foo") : nothing
    struct     bar <:    Int
    end
    for i=1:1_000
      nothing
    end
    function hello(::T) where T<:Int
     println("No")
        println("Indentation")
    println("shall")
     println("stop me.")
        end
    a  +
      b
    3+1:9*5
    
    """
    filepath, io = mktemp()
    write(io, bad_bad_code)
    close(io)
    @compile_workload begin
        redirect_stdio(stdout = devnull, stderr = devnull) do
            # Format this bad bad code
            main(["-v", "-o", tempname(), filepath])
            # Help print
            main(["--help"])
            # If git is available
            git = Sys.which("git")
            if !isnothing(git)
                main(["--diff", "--check", filepath])
            end
        end
    end
end
