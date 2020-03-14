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

EXTRACTED=cpu_v.ml cpu_v.mli

PROOFS=\
	U4.vo  \
	U8.vo  \
	U16.vo \
	Cpu_v.vo

OBJECTS=\
	types.cmx 	 \
	cartridge.cmx\
	input.cmx    \
	sound.cmx    \
	gpu.cmx      \
	timer.cmx 	 \
	system.cmx 	 \
	cpu_v.cmx    \
	cpu_ml.cmx   \
	main.cmx     \

# Toplevel.
all:
	$(MAKE) all-v
	$(MAKE) all-ml


# Coq build.
all-v:
	test -f .depend.v || $(MAKE) depend-v
	$(MAKE) proof
	$(MAKE) $(EXTRACTED)

depend-v:  $(PROOFS:.vo=.v)
	$(COQDEP) $^ > .depend.v

proof: $(PROOFS)

$(EXTRACTED): $(PROOFS) extraction.v
	$(COQEXEC) extraction.v


# OCaml build.
all-ml:
	test -f .depend.ml || $(MAKE) depend-ml
	$(MAKE) gbml

depend-ml: $(OBJECTS:.cmx=.ml) $(OBJECTS:.cmx=.mli)
	$(OCAMLDEP) $^ > .depend.ml

gbml: $(OBJECTS)
	$(OCAMLFIND) $(OCAMLOPT) -o $@ -linkpkg -package $(PACKAGES) $^

# Build rules for all languages.
%.vo: %.v
	$(COQC) $*.v

%.cmi: %.mli
	$(OCAMLFIND) $(OCAMLC) -c $< -package $(PACKAGES)

%.cmx: %.ml %.cmi
	$(OCAMLFIND) $(OCAMLOPT) -c $< -package $(PACKAGES)

cpu_v.cmx: cpu_v.ml cpu_v.cmi
	$(OCAMLFIND) $(OCAMLOPT) -c $< -package $(PACKAGES) -w -8

# Clean.
.PHONY: clean
clean:
	rm -f .depend.v .depend.ml
	rm -f *.vo *.vok *.vos *.glob .*.aux .merlin
	rm -f *.o *.cmi *.cmo *.cmx
	rm -f cpu_v.ml cpu_v.mli


-include .depend.v
-include .depend.ml
