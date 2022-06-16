include Makefile.include

include $(FSTAR_ULIB)/ml/Makefile.include

# all: hello testseq multi

# browserIO: out browserIO.fst
# 	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --odir out --codegen OCaml --extract 'BrowserIO' BrowserIO.fst --record_hints
# 	# $(OCAMLOPT) out/BrowserIO.ml -o BrowserIO.exe
# 	# ./BrowserIO.exe

browser: out browser.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) Assumed.fst BrowserIO.fst --odir out --codegen OCaml --extract 'Browser' Browser.fst --record_hints



out:
	mkdir -p out

clean:
	rm -rf out *~ *.exe