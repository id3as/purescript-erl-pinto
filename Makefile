.PHONY: ps ps_test erl all test clean

all: ps

ps:
	spago build


test:
	spago -x test.dhall build
	rebar3 compile

clean:
	rm -rf ebin output src/compiled_ps
