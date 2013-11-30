
test: MOO/Builtins/Match.hs FORCE
	ghc --make test.hs -lcrypt -lpcre

FORCE:

MOO/Builtins/Match.hs: MOO/Builtins/Match.hsc
	hsc2hs $<
