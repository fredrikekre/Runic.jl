module RunicC

using Runic: Runic

# TODO: Why do we need this shim? Wouldn't it be possible to just compile `src/Runic.jl`?
Base.@ccallable function main()::Cint
    argv = String[]
    return Runic.main(argv)
end

end
