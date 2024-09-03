JULIA ?= /opt/julia/julia-c/bin/julia
JULIAC ?= $(shell $(JULIA) -e 'print(normpath(joinpath(Sys.BINDIR, Base.DATAROOTDIR, "julia", "juliac.jl")))')

RUNIC_FILES := $(wildcard ../src/*.jl) ../Project.toml

runicc: runicc.jl Project.toml Manifest.toml $(RUNIC_FILES)
	$(JULIA) --project=. $(JULIAC) --output-exe $@ --trim=unsafe-warn $<

Manifest.toml: Project.toml ../Project.toml
	$(JULIA) --project=. -e 'using Pkg; Pkg.instantiate()'
	@touch $@ # Pkg.instantiate doesn't update the mtime if there are no changes

clean:
	-rm -f runicc Manifest.toml

print-%:
	@echo '$*=$($*)'

.PHONY: clean
