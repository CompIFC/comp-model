include ../Makefile.include

include $(FSTAR_ULIB)/ml/Makefile.include

# all: hello testseq multi

hello: out Hello.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --odir out --codegen OCaml --extract 'Hello' Hello.fst --record_hints
	$(OCAMLOPT) out/Hello.ml -o hello.exe
	./hello.exe


out:
	mkdir -p out

clean:
	rm -rf out *~ *.exe
