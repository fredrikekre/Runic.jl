JULIA ?= $(shell which julia)
JULIAC ?= $(shell which juliac)

RUNIC_FILES := $(wildcard ../src/*.jl) ../Project.toml

runicc: runicc.jl Project.toml Manifest.toml $(RUNIC_FILES)
	$(JULIAC) --project=. --verbose --output-exe $@ --experimental --trim=safe $<

Manifest.toml: Project.toml ../Project.toml
	$(JULIA) --project=. -e 'using Pkg; Pkg.instantiate()'
	@touch $@ # Pkg.instantiate doesn't update the mtime if there are no changes

clean:
	-rm -f runicc Manifest.toml

print-%:
	@echo '$*=$($*)'

.PHONY: clean
