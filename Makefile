
repl: FORCE MOO/Builtins/Match.hs
	ghc $(OPT) --make repl.hs -lcrypt -lpcre

FORCE:

MOO/Builtins/Match.hs: MOO/Builtins/Match.hsc
	hsc2hs $<

.PHONY: clean
clean:
	find * \( -name \*.o        \
	       -o -name \*.o-boot   \
	       -o -name \*.hi       \
	       -o -name \*.hi-boot  \
	       \) -print0 | xargs -0 rm
