MAKEFLAGS += --silent
FLAGS = \
	-g \
	-nolabels \
	-strict-formats \
	-strict-sequence \
	-unboxed-types \
	-warn-error "+a"
SOURCE = \
	prelude.ml \
	main.ml

.PHONY: all
all: bin/main

.PHONY: clean
clean:
	rm -rf bin/
	rm -rf build/

.PHONY: run
run: bin/main
	bin/main

bin/main: src/*.ml
	mkdir -p bin/
	mkdir -p build/
	ocp-indent -i src/*.ml
	cp src/*.ml build/
	cd build/; ocamlc $(FLAGS) -o ../bin/main $(SOURCE)
