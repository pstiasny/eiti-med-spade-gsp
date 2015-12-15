main: *.hs
	ghc main


graph.ps: main
	./main > ggraph.dot
	dot -Tps ggraph.dot -o graph.ps
