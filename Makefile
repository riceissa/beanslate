beanslate: Beanslate.hs
	ghc -o beanslate Beanslate.hs

.PHONY: clean
clean:
	rm beanslate Beanslate.hi Beanslate.o
