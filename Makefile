RESULT = tsg
SOURCES = tsg.ml
PACKS = re.pcre,graphics,extlib
INCDIRS =
LIBS =
THREADS = yes
ANNOTATE = yes
OCAMLFLAGS = -bin-annot -w A

all: native-code

export OCAMLMAKEFILE = ~/src/ocamlmakefile/OCamlMakefile
include $(OCAMLMAKEFILE)
