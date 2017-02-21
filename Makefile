mcsvf: mcs.ml mcs.cmi mcsvf.ml
	ocamlopt -o mcsvf mcs.ml mcsvf.ml

mcs.cmi: mcs.mli
	ocamlc -c mcs.mli

clean:
	rm *.cmi *.cmx *.o mcsvf *.out
