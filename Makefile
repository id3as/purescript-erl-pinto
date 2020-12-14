.PHONY: ps ps_test erl all test clean

all: ps

ps:
	spago build

ps_test:
	spago --config test.dhall build

test: ps_test erl
	erl -pa ebin -noshell -eval '(test_main@ps:main())()' -eval 'init:stop()'

erl:
	mkdir -p ebin
	erlc -o ebin/ output/*/*.erl

clean:
	rm -rf ebin output src/compiled_ps
