
NAME=osam

LIBSRC = Cursor.ml Mark.ml Text.ml Patch.ml Regexp.ml Syscmd.ml Action.ml
LIB = $(patsubst %.ml,%.cma,$(LIBSRC))
LIBX = $(patsubst %.ml,%.cmxa,$(LIBSRC))
LIBOBJ = $(patsubst %.ml,%.cmo,$(LIBSRC))
LIBXOBJ = $(patsubst %.ml,%.cmx,$(LIBSRC))
LIBCMI = $(patsubst %.ml,%.cmi,$(LIBSRC))
LIBA = $(patsubst %.ml,%.a,$(LIBSRC))

SRC = osam.ml
OBJ = $(patsubst %.ml,%.cmo,$(SRC))
XOBJ = $(patsubst %.ml,%.cmx,$(SRC))
EXEC = $(patsubst %.ml,%,$(SRC))

REQUIRES=-package uutf

all: $(LIB) $(LIBX) $(EXEC)

#.PHONY: install
#install: all
#	ocamlfind install $(NAME) $(LIB) $(LIBCMI) $(LIBA) $(LIBX) META

#.PHONY: uninstall
#uninstall:
#	ocamlfind remove $(NAME)

$(EXEC): $(LIBXOBJ) $(XOBJ)
	ocamlfind ocamlopt -linkpkg $(REQUIRES) -o $@ $(LIBXOBJ) $(XOBJ)

$(LIB): $(LIBCMI) $(LIBOBJ)
	ocamlfind ocamlc $(REQUIRES) -a -o $@ $(LIBOBJ)

$(LIBX): $(LIBCMI) $(LIBXOBJ)
	ocamlfind ocamlopt $(REQUIRES) -a -o $@ $(LIBXOBJ)

%.cmo: %.ml
	ocamlfind ocamlc $(REQUIRES) -c $<

%.cmi: %.mli
	ocamlfind ocamlc $(REQUIRES) -c $<

%.cmx: %.ml
	ocamlfind ocamlopt $(REQUIRES) -c $<

.PHONY: clean
clean:
	rm -f *.cm[oixa] *.cmxa *.o *.a \
	  $(EXEC)

#Dependencies
Action.cmo : Text.cmi Syscmd.cmi Regexp.cmi Patch.cmi Mark.cmi Cursor.cmi \
    Action.cmi
Action.cmx : Text.cmx Syscmd.cmx Regexp.cmx Patch.cmx Mark.cmx Cursor.cmx \
    Action.cmi
Action.cmi : Text.cmi Syscmd.cmi Regexp.cmi Patch.cmi Mark.cmi Cursor.cmi
Cursor.cmo : Cursor.cmi
Cursor.cmx : Cursor.cmi
Cursor.cmi :
Mark.cmo : Cursor.cmi Mark.cmi
Mark.cmx : Cursor.cmx Mark.cmi
Mark.cmi : Cursor.cmi
osam.cmo : Text.cmi Regexp.cmi Patch.cmi Cursor.cmi Action.cmi
osam.cmx : Text.cmx Regexp.cmx Patch.cmx Cursor.cmx Action.cmx
Patch.cmo : Text.cmi Cursor.cmi Patch.cmi
Patch.cmx : Text.cmx Cursor.cmx Patch.cmi
Patch.cmi : Text.cmi Cursor.cmi
Regexp.cmo : Text.cmi Cursor.cmi Regexp.cmi
Regexp.cmx : Text.cmx Cursor.cmx Regexp.cmi
Regexp.cmi : Text.cmi Cursor.cmi
Syscmd.cmo : Syscmd.cmi
Syscmd.cmx : Syscmd.cmi
Syscmd.cmi :
Text.cmo : Cursor.cmi Text.cmi
Text.cmx : Cursor.cmx Text.cmi
Text.cmi : Cursor.cmi
