make:
	ghc lifegame.hs -rtsopts -threaded -Odph -fllvm -optlo-O3
test:
	./lifegame +RTS -N2 -K1024000000
clean:
	rm -rf lifegame lifegame.o lifegame.hi
