OCBFLAGS :=
OCB := ocamlbuild $(OCBFLAGS)

.PHONY: all debug clean top profile gen-baselines check-baselines

all: synml.native
debug: all synml.cma

%.cma: .FORCE
	$(OCB) $@

%.cmxa: .FORCE
	$(OCB) $@

%.native: .FORCE
	$(OCB) $@

%.p.native: .FORCE
	$(OCB) $@

%.byte: .FORCE
	$(OCB) $@

.FORCE:

clean:
	$(OCB) -clean

top: synml.cma
	utop

CHECK_BASELINE := python check-baseline.py

check-baselines: synml.native
	$(CHECK_BASELINE) ./synml.native tests/ast
	$(CHECK_BASELINE) ./synml.native tests/bool
	$(CHECK_BASELINE) ./synml.native tests/nat
	$(CHECK_BASELINE) ./synml.native tests/natlist
	$(CHECK_BASELINE) ./synml.native tests/tree

GENERATE_DATA := python generate-data.py

generate-data: synml.native
	@$(GENERATE_DATA) ./synml.native tests/ast
	@$(GENERATE_DATA) ./synml.native tests/bool
	@$(GENERATE_DATA) ./synml.native tests/nat
	@$(GENERATE_DATA) ./synml.native tests/natlist
	@$(GENERATE_DATA) ./synml.native tests/tree

profile: synml_profile.p.native
	instruments -t "/Applications/Xcode.app/Contents/Applications/Instruments.app/Contents/Resources/templates/Time Profiler.tracetemplate" synml_profile.p.native
