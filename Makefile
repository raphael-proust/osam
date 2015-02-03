
NAME=OSam

SRC = Cursor.ml Mark.ml Patch.ml Text.ml sam.ml
LIB = $(patsubst %.ml,%.cma,$(SRC))
LIBX = $(patsubst %.ml,%.cmxa,$(SRC))
LIBOBJ = $(patsubst %.ml,%.cmo,$(SRC))
LIBXOBJ = $(patsubst %.ml,%.cmx,$(SRC))
LIBCMI = $(patsubst %.ml,%.cmi,$(SRC))
LIBA = $(patsubst %.ml,%.a,$(SRC))

REQUIRES= #when not empty, add --package "$(REQUIRES)" to build rules

all: $(LIB) $(LIBX)

#.PHONY: install
#install: all
#	ocamlfind install $(NAME) $(LIB) $(LIBCMI) $(LIBA) $(LIBX) META

#.PHONY: uninstall
#uninstall:
#	ocamlfind remove $(NAME)

$(LIB): $(LIBCMI) $(LIBOBJ)
	ocamlfind ocamlc -a -o $@ $(LIBOBJ)

$(LIBX): $(LIBCMI) $(LIBXOBJ)
	ocamlfind ocamlopt -a -o $@ $(LIBXOBJ)

%.cmo: %.ml
	ocamlfind ocamlc -c $(INCLUDES) $<

%.cmi: %.mli
	ocamlfind ocamlc -c $(INCLUDES) $<

%.cmx: %.ml
	ocamlfind ocamlopt -c $(INCLUDES) $<

.PHONY: clean
clean:
	rm -f *.cmo *.cmx *.cmi *.o \
	  $(LIB) $(LIBX) $(LIBA)

#Dependencies
Cursor.cmo : Cursor.cmi
Cursor.cmx : Cursor.cmi
Cursor.cmi :
Mark.cmo : Cursor.cmi Mark.cmi
Mark.cmx : Cursor.cmx Mark.cmi
Mark.cmi : Cursor.cmi
Patch.cmo : Text.cmi Cursor.cmi Patch.cmi
Patch.cmx : Text.cmx Cursor.cmx Patch.cmi
Patch.cmi : Text.cmi Cursor.cmi
sam.cmo : Text.cmi Patch.cmi Mark.cmi Cursor.cmi sam.cmi
sam.cmx : Text.cmx Patch.cmx Mark.cmx Cursor.cmx sam.cmi
sam.cmi :
Text.cmo : Cursor.cmi Text.cmi
Text.cmx : Cursor.cmx Text.cmi
Text.cmi : Cursor.cmi
