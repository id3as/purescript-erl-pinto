.PHONY: all test runtest erl ci clean cleandist formatPS

PS_SRC = src
TEST_SRC = test

PS_SOURCEFILES = $(shell find ${PS_SRC} -type f -name \*.purs)
PS_ERL_FFI = $(shell find ${PS_SRC} -type f -name \*.erl)
PS_TEST_SOURCEFILES = $(shell find ${TEST_SRC} -type f -name \*.purs)
PS_TEST_ERL_FFI = $(shell find ${TEST_SRC} -type f -name \*.erl)


.DEFAULT_GOAL := erl

all: erl docs

ci: all test

output/.complete: .spago $(PS_SOURCEFILES) $(PS_ERL_FFI)
	spago build
	touch output/.complete

output/.testcomplete: .spago $(PS_SOURCEFILES) $(PS_ERL_FFI) $(PS_TEST_SOURCEFILES) $(PS_TEST_ERL_FFI)
	# Should be able to just use the below, but spago does not pass the testouput directory through to the purs backend
	spago -x test.dhall build --purs-args "--censor-codes=ShadowedName,WildcardInferredType"
	touch output/.testcomplete

docs: output/.complete
	mkdir -p docs
	spago docs --format markdown
	cp generated-docs/md/Erl.Quorum*.md docs

.spago: spago.dhall test.dhall packages.dhall
	spago install
	spago -x test.dhall install
	touch .spago

erl: output/.complete
	rebar3 as dist compile

test: rebar.lock output/.testcomplete
	rebar3 as test_profile compile
	erl -pa ebin -pa $$(rebar3 as test_profile path) -noshell -sname "runner" -eval '(test_main@ps:main())()' -eval 'init:stop()'

formatPS:
	purs-tidy format-in-place src/ test/

clean:
	rebar3 as dist clean
	rebar3 as test_profile clean
	rm -rf output

distclean: clean
	rm -rf .spago _build

runtest:
	erl -pa ebin -pa $$(rebar3 as test_profile path) -noshell -sname "runner" -eval '(test_main@ps:main())()' -eval 'init:stop()'

# Rebar3 won't generate this under profiles
# and without it, our paths are wrong
rebar.lock: rebar.config
	rebar3 get-deps
