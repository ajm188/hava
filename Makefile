MAIN=-main-is
TARGETS=Main Lex Parse

all: $(TARGETS)

Main: Main.hs Lex.hs Parse.hs
	ghc $@

Lex: Lex.hs IOUtils.hs
	ghc $(MAIN) $@ $<

Parse: Parse.hs IOUtils.hs
	ghc $(MAIN) $@ $<

clean:
	rm -f *.o
	rm -f *.hi
	rm -f $(TARGETS)
