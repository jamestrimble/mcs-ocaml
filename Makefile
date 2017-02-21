mcsvf: mcs.ml mcsvf.ml
	ocamlopt -o mcsvf mcs.ml mcsvf.ml
clean:
	rm *.cmi *.cmx *.o mcsvf *.out
