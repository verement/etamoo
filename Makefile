
repl: FORCE MOO/Builtins/Match.hs
	ghc $(OPT) --make -threaded -rtsopts repl.hs -lcrypt -lpcre

MOO/Builtins/Match.hs: MOO/Builtins/Match.hsc
	hsc2hs $<

hlint.html: $(shell find . -name \*.hs \! -name .\*)
	-hlint --report=$@ $^ >/dev/null

.PHONY: clean
clean:
	find * \( -name \*.o        \
	       -o -name \*.o-boot   \
	       -o -name \*.hi       \
	       -o -name \*.hi-boot  \
	       \) -print0 | xargs -0 rm

FORCE:
