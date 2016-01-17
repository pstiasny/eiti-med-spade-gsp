main: *.hs
	ghc main


graph.ps: main
	./main -g < ../db.horizontal > ggraph.dot
	dot -Tps ggraph.dot -o graph.ps
