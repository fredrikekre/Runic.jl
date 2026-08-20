JULIA ?= $(shell which julia)

RUNIC_FILES := $(wildcard ../src/*.jl) ../Project.toml

# Flags passed to JuliaC. Override to e.g. disable trimming: `make JULIAC_FLAGS=`
JULIAC_FLAGS ?= --experimental --trim=safe

runicc: runicc.jl Project.toml Manifest.toml $(RUNIC_FILES) | check-julia
	$(JULIA) --project=. -m JuliaC --verbose --output-exe $@ --project=. $(JULIAC_FLAGS) $<

# Note: a Manifest.toml from before JuliaC was a dependency makes `Pkg.instantiate()`
# error; remove it (e.g. `make clean`) and rerun.
Manifest.toml: Project.toml ../Project.toml | check-julia
	$(JULIA) --project=. -e 'using Pkg; Pkg.instantiate()'
	@touch $@ # Pkg.instantiate doesn't update the mtime if there are no changes

clean:
	-rm -f runicc Manifest.toml

check-julia:
	@if ! $(JULIA) -e 'exit(VERSION >= v"1.12" ? 0 : 1)' 2> /dev/null; then \
		echo "ERROR: The configured julia binary ($(JULIA)) is missing or too old (julia 1.12+ is required)."; \
		echo "       Configure the binary using the JULIA variable (e.g. \`JULIA=/path/to/julia make ...\`)"; \
		echo "       or change how \`julia\` resolves in \`PATH\` (e.g. \`juliaup default 1.13\`)."; \
		exit 1; \
	fi

print-%:
	@echo '$*=$($*)'

.PHONY: clean check-julia
