
all: test.cmo util.cmo 

reactive.cmo: reactive.ml
	ocamlc -c reactive.ml

domain.cmo: domain.ml
	ocamlc -c domain.ml

level.cmo: level.ml domain.cmo
	ocamlc -c level.ml

io.cmo: io.ml domain.cmo level.cmo
	ocamlc -c io.ml

browser.cmo: browser.ml reactive.cmo io.cmo domain.cmo
	ocamlc -c browser.ml

test.cmo: test.ml browser.cmo policyfun.cmo wrapper.cmo domain.cmo io.cmo reactive.cmo level.cmo
	ocamlc -c test.ml 

util.cmo: util.ml
	ocamlc -c util.ml

policyfun.cmo: policyfun.ml domain.cmo io.cmo level.cmo
	ocamlc -c policyfun.ml 

policy.cmo: policy.ml policyfun.ml io.ml level.cmo
	ocamlc -c policy.ml

wrapper.cmo: wrapper.ml reactive.cmo browser.cmo policyfun.cmo policy.cmo level.cmo
	ocamlc -c wrapper.ml 

doc: reactive.cmo domain.cmo io.cmo browser.cmo  policyfun.cmo policy.cmo wrapper.cmo
	if [ ! -d doc ]; then mkdir doc; fi
	ocamldoc -html -hide Domain -hide Io -d doc reactive.ml domain.ml io.ml browser.ml policyfun.ml policy.ml wrapper.ml
	cat doc/style.css doc.css > tmp.css
	mv tmp.css doc/style.css
	sed 's/<br>      //g' < doc/Reactive.REACTIVE_SYSTEM_TYPE.html > tmp.html
	mv tmp.html doc/Reactive.REACTIVE_SYSTEM_TYPE.html
	sed 's/<br>      //g' < doc/Io.html > tmp.html
	mv tmp.html doc/Io.html
	sed 's/<br>      //g' < doc/Browser.Impl.html > tmp.html
	mv tmp.html doc/Browser.Impl.html

latex-doc: reactive.cmo io.cmo browser.cmo
	if [ ! -d latex-doc ]; then mkdir latex-doc; fi
	ocamldoc -latex -hide Domain -hide Io -o main.tex reactive.ml domain.ml io.ml browser.ml 
	mv main.tex ocamldoc.sty latex-doc/

clean:
	rm -rf main *.cmo *.cmi doc latex-doc main.tex ocamldoc.sty ocamldoc.out *.aux *.log *.dvi

.PHONY: all clean

