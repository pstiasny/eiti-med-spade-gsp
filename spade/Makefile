main: *.hs
	ghc main


graph.ps: main
	./main -g < ../db.horizontal > ggraph.dot
	dot -Tps ggraph.dot -o graph.ps


test: main
	./main < ../db.horizontal | diff TESTOUT -
