# Copyright (c) 2009, Benedikt Meurer <benedikt.meurer@googlemail.com>
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
#
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.

OCAMLC=ocamlc
OCAMLCFLAGS=-g -warn-error A
OCAMLDEP=ocamldep
OCAMLDEPFLAGS=
OCAMLOPT=ocamlopt
OCAMLOPTFLAGS=$(OCAMLCFLAGS)

SOURCES=		\
	rbmap.ml	\
	rbmap.mli	\
	rbset.ml	\
	rbset.mli

OBJECTS=$(patsubst %.ml,%.cmo,$(patsubst %.mli,%.cmi,$(SOURCES)))	\
	$(patsubst %.ml,%.cmx,$(patsubst %.mli,%.cmi,$(SOURCES)))

all: $(OBJECTS)

clean:
	rm -f .depend *.cmi *.cmo *.cmx *.o

.SUFFIXES: .cmi .cmo .cmx .ml .mli

.ml.cmo:
	$(OCAMLC) $(OCAMLCFLAGS) -c -o $@ $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c -o $@ $<

.mli.cmi:
	$(OCAMLC) $(OCAMLCFLAGS) -c -o $@ $<

.depend: Makefile $(SOURCES)
	$(OCAMLDEP) $(OCAMLDEPFLAGS) $(SOURCES) > $@

include .depend

.PHONY: clean
