# This file is part of the GBC-ML project.
# Licensing information is available in the LICENSE file.
# (C) 2020 Nandor Licker. All rights reserved.

FILES=Cpu.v

COQINCLUDES=-R . GB
COQCOPTS ?= -w -undeclared-scope

COQC=$(COQBIN)coqc -q $(COQINCLUDES) $(COQCOPTS)
COQDEP=$(COQBIN)coqdep $(COQINCLUDES)
COQEXEC=$(COQBIN)coqtop $(COQINCLUDES) -batch -load-vernac-source

OCAMLC=ocamlc $(COMPFLAGS)
OCAMLOPT=ocamlopt $(COMPFLAGS)
OCAMLDEP=ocamldep -slash $(INCLUDES)
OCAMLFIND=ocamlfind

PACKAGES=unix,graphics

EXTRACTED=cpu.ml cpu.mli

OBJECTS=\
	types.cmx 	 \
	cartridge.cmx\
	input.cmx    \
	sound.cmx    \
	gpu.cmx      \
	timer.cmx 	 \
	system.cmx 	 \
	cpu.cmx      \
	main.cmx     \

# Toplevel.
all:
	$(MAKE) all-v
	$(MAKE) all-ml


# Coq build.
all-v:
	$(MAKE) proof
	$(MAKE) $(EXTRACTED)

proof: $(FILES:.v=.vo)

$(EXTRACTED): extraction.v
	$(COQEXEC) extraction.v


# OCaml build.
all-ml:
	$(MAKE) gbml

gbml: $(OBJECTS)
	$(OCAMLFIND) $(OCAMLOPT) -o $@ -linkpkg -package $(PACKAGES) $^

# Build rules for all languages.
%.vo: %.v
	$(COQC) $*.v

%.cmi: %.mli
	$(OCAMLFIND) $(OCAMLC) -c $< -package $(PACKAGES)

%.cmx: %.ml %.cmi
	$(OCAMLFIND) $(OCAMLOPT) -c $< -package $(PACKAGES)

cpu.cmx: cpu.ml cpu.cmi
	$(OCAMLFIND) $(OCAMLOPT) -c $< -package $(PACKAGES) -w -8

main.cmx: main.ml
	$(OCAMLFIND) $(OCAMLOPT) -c $< -package $(PACKAGES)

# Clean.
.PHONY: clean
clean:
	rm -f *.vo *.vok *.vos *.glob .*.aux .merlin
	rm -rf *.o *.cmi *.cmo *.cmx
	rm -f cpu.ml cpu.mli


-include .depend.ml
-include .depend.v
