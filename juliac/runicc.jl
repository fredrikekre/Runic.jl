module RunicC

    using Runic: Runic

    function @main(argv::Vector{String})::Cint
        return Runic.main(argv)
    end

end

using .RunicC: main
