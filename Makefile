
default: FORCE etamoo

test: FORCE etamoo-test

CABAL         = EtaMOO.cabal

HS_SOURCES    = $(shell find src -name \*.hs  \! -name .\*)
HSC_SOURCES   = $(shell find src -name \*.hsc \! -name .\*)
SOURCES       = $(HS_SOURCES) $(HSC_SOURCES)

BUILT_SOURCES = $(foreach file,$(HSC_SOURCES),$(patsubst  \
		src/%.hsc,dist/.test/build/etamoo/etamoo-tmp/%.hs,$(file)))

DOCS          = dist/doc/html/EtaMOO/etamoo/index.html

HACKAGE       = hackage.haskell.org
HACKAGE_DOCS  = http://$(HACKAGE)/packages/archive/$$pkg/latest/doc/html

BUILD         = cabal build -j

etamoo: FORCE
	@$(BUILD) --builddir=dist
	ln -sf dist/build/etamoo/etamoo $@

etamoo-test: FORCE
	@$(BUILD) --builddir=dist/.test
	ln -sf dist/.test/build/etamoo/etamoo $@

etamoo-prof: FORCE
	@$(BUILD) --builddir=dist/.prof
	ln -sf dist/.prof/build/etamoo/etamoo $@

etamoo-llvm: FORCE
	@$(BUILD) --builddir=dist/.llvm
	ln -sf dist/.llvm/build/etamoo/etamoo $@

$(BUILT_SOURCES): $(HSC_SOURCES)
	$(MAKE) test

hlint.html: $(HS_SOURCES) $(BUILT_SOURCES)
	-hlint --report=$@ $^ >/dev/null

$(DOCS): $(CABAL) $(SOURCES) Makefile
	@cabal haddock --builddir=dist  \
		--html-location='$(HACKAGE_DOCS)'  \
		--haddock-options="--title=EtaMOO --no-warnings"  \
		--executables --hyperlink-source

docs: FORCE $(DOCS)

%.ps: %.hp
	hp2ps $^

install: FORCE
	@cabal install --builddir=dist

clean: FORCE
	-@cabal clean --builddir=dist
	rm -f etamoo etamoo-* *.prof *.aux *.hp *.ps hlint.html

FORCE:
