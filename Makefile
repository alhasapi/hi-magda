magda: src/*.hs
	mkdir -p bin
	cd src && ghc Interpreter.hs -outputdir ../bin -o ../bin/magda

install: bin/magda
	cp -v bin/magda /usr/local/bin/magda

clean:
	rm -fv -R bin src/*~

.PHONY: magda

