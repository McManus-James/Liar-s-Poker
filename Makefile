test:
	ocamlbuild -pkgs oUnit,str,unix test_main.byte && ./test_main.byte

play:
	ocamlbuild -pkgs oUnit,str,unix main.byte && ./main.byte

clean:
	ocamlbuild -clean
	rm -f _build