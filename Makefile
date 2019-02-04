.PHONY: all clean test

PS_SRC = src
OUTPUT = output

all: output

output: $(PS_SRC)/**/*.purs $(PS_SRC)/**/*.erl $(PS_SRC)/*.purs .psc-package
	.psc-package/erl-0.12.1-20181214/erl-modules/v0.1.1/scripts/gen_module_names.sh src/
	psc-package sources | xargs purs compile '$(PS_SRC)/**/*.purs'
	@touch output

.psc-package: psc-package.json
	psc-package install
	touch .psc-package

clean:
	rm -rf $(OUTPUT)/*
