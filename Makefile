mcsvf: mcs.ml mcsvf.ml
	ocamlopt -o mcsvf mcs.ml mcsvf.ml
clean:
	rm *.cmo *.cmx *.o mcsvf *.out
