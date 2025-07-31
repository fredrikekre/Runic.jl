using Runic: Runic

# Compileable main function corresponding to `int main(int argc, char** argv)`
function (@main)(args::Vector{String})::Cint
    if length(args) == 1
        Runic.print_help()
        return 0
    else
        return Runic.main(args[2:end])
    end
end
