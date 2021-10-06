.PHONY: ps ps_test erl all test clean

all: ps

ps:
	spago build


test:
	spago -x test.dhall test

clean:
	rm -rf ebin output src/compiled_ps
