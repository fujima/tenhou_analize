SOURCES = lib/myStdlib.ml tp/syntax.ml tp/tenhou.ml test/main.ml
LIBS = str xml-light
PACKS = netstring
RESULT  = a.out

.PHONY: all test

all: byte-code test

test: 
	gunzip "2011083123gm-00e1-0000-fac2e4e3&tw=0.mjlog" -c | ./a.out

include OCamlMakefile
