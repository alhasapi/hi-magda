magda: src/*.hs
	mkdir bin
	cd src && ghc Interpreter.hs -o ../bin/magda

clean:
	rm -v -R src/*.hi src/*.o bin

.PHONY: magda

