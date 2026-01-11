build: main.hs
	mkdir -p dist
	ghc main.hs -odir dist -hidir dist -outputdir dist -o dist/todo

clean:
	rm -rf dist
