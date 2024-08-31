JULIA ?= /opt/julia/julia-c/bin/julia
JULIAC ?= $(shell $(JULIA) -e 'print(normpath(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "juliac.jl")))')
RUNIC_SRCFILES := $(wildcard ../src/*.jl)

runicc: runicc.jl $(RUNIC_SRCFILES) invalidate-precompile-cache
	$(JULIA) $(JULIAC) --output-exe $@ --trim=unsafe-warn $<

clean:
	rm runicc

# Prune cached precompile files for Runic. This is needed because there are
# (compile time) branches in the Runic source code which depends on whether
# Runic is compiled or not. It looks like juliac will use existing cache files
# but not produce any so there is no need to prune them again after compilation
# to force regular usage to recompile.
invalidate-precompile-cache:
	$(JULIA) -e ' \
		ji = Base.compilecache_path(Base.PkgId(Base.UUID("62bfec6d-59d7-401d-8490-b29ee721c001"), "Runic")); \
		if ji !== nothing; \
			isfile(ji) && (@info "Deleting precompile file $$(ji)"; rm(ji)); \
			so = splitext(ji)[1] * "." * Base.BinaryPlatforms.platform_dlext(); \
			isfile(so) && (@info "Deleting pkgimage file $$(so)"; rm(so)); \
		end'


.PHONY: invalidate-precompile-cache clean
