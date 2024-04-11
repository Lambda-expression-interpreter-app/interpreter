all: build run clean

build:
	ghc Main.hs -o main -O2

run:
	./main $(mode)

.PHONY: clean
clean:
	rm -rf main *.o *.hi