.PHONY: all clean test

PS_SRC = src
OUTPUT = output
PS_SOURCEFILES = $(shell find ${PS_SRC} -type f -name \*.purs)
PS_ERL_FFI = $(shell find ${PS_SRC} -type f -name \*.erl)

PACKAGE_SET = $(shell jq '.set' < psc-package.json)
ERL_MODULES_VERSION = $(shell jq '."erl-modules".version' < .psc-package/$(PACKAGE_SET)/.set/packages.json)

all: output

output: $(PS_SOURCEFILES) $(PS_ERL_FFI) .psc-package
	.psc-package/${PACKAGE_SET}/erl-modules/${ERL_MODULES_VERSION}/scripts/gen_module_names.sh src/Pinto Pinto.ModuleNames
	psc-package sources | xargs purs compile '$(PS_SRC)/**/*.purs'
	@touch output

.psc-package: psc-package.json
	psc-package install
	touch .psc-package

clean:
	rm -rf $(OUTPUT)/*
