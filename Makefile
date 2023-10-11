MAKEFLAGS += --silent
FLAGS = \
	-fprof-auto \
	-fprof-cafs \
	-prof \
	-fdiagnostics-color=always \
	-isrc \
	-outputdir build \
	-Wall \
	-Wcompat \
	-Werror \
	-Widentities \
	-Wincomplete-record-updates \
	-Wincomplete-uni-patterns \
	-Wmonomorphism-restriction \
	-Wpartial-fields \
	-Wredundant-constraints \
	-Wunused-packages \
	-Wunused-type-patterns
MODULES = \
	Ast \
	Escape \
	Main \
	Parse
LINTS = $(foreach x,$(MODULES),build/$(x).lint)

.PHONY: all
all: bin/main

.PHONY: clean
clean:
	rm -rf bin/
	rm -rf build/

.PHONY: run
run: all
	./bin/main +RTS -xc -RTS < ex/counter.fl

$(LINTS): build/%.lint: src/%.hs
	mkdir -p build/
	hlint $^
	ormolu -i --no-cabal $^
	touch $@

bin/main: $(LINTS)
	mkdir -p bin/
	ghc $(FLAGS) -o bin/main src/Main.hs
