test:
	ocamlbuild -pkgs oUnit,str,unix test_main.byte && ./test_main.byte

clean:
	ocamlbuild -clean
	rm -f _build