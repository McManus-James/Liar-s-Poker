test:
	ocamlbuild -pkgs oUnit,str,unix test_deck.byte && ./test_deck.byte

clean:
	ocamlbuild -clean
	rm -f _build