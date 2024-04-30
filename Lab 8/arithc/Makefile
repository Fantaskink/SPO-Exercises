
compile: main.exe
	./main.exe test.exp
	less test.s

run: main.exe
	./main.exe test.exp
	spim -file test.s

main.exe:
	dune build main.exe

clean:
	dune clean
	rm -f test.s .depend

.PHONY: clean main.exe
