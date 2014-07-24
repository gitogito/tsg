RESULT = a.out
SOURCES = a.ml
PACKS = pcre,graphics,extlib
INCDIRS =
LIBS =
THREADS = yes
ANNOTATE = yes
OCAMLFLAGS = -bin-annot -w A

all: debug-code

export OCAMLMAKEFILE = ~/src/ocamlmakefile/OCamlMakefile
include $(OCAMLMAKEFILE)
