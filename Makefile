magda: src/*.hs
	mkdir bin
	cd src && ghc Interpreter.hs -o ../bin/magda

clean:
	rm -fv -R src/*.hi src/*.o bin src/*~

.PHONY: magda

