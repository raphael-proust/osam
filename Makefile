
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

REQUIRES= #when not empty, add --package "$(REQUIRES)" to build rules
INCLUDES=

all: $(LIB) $(LIBX) $(EXEC)

#.PHONY: install
#install: all
#	ocamlfind install $(NAME) $(LIB) $(LIBCMI) $(LIBA) $(LIBX) META

#.PHONY: uninstall
#uninstall:
#	ocamlfind remove $(NAME)

$(EXEC): $(LIBXOBJ) $(XOBJ)
	ocamlfind ocamlopt -o $@ $<

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
Patch.cmo : Text.cmi Cursor.cmi Patch.cmi
Patch.cmx : Text.cmx Cursor.cmx Patch.cmi
Patch.cmi : Text.cmi Cursor.cmi
Regexp.cmo : Regexp.cmi
Regexp.cmx : Regexp.cmi
Regexp.cmi : Text.cmi Cursor.cmi
osam.cmo : Text.cmi Patch.cmi Mark.cmi Cursor.cmi Action.cmi
osam.cmx : Text.cmx Patch.cmx Mark.cmx Cursor.cmx Action.cmx
Syscmd.cmo : Syscmd.cmi
Syscmd.cmx : Syscmd.cmi
Syscmd.cmi :
Text.cmo : Cursor.cmi Text.cmi
Text.cmx : Cursor.cmx Text.cmi
Text.cmi : Cursor.cmi
