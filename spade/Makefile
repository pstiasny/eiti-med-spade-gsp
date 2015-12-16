main: *.hs
	ghc main


graph.ps: main
	./main -g > ggraph.dot
	dot -Tps ggraph.dot -o graph.ps
