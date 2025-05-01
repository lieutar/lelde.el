#>Makefile for the package {{name}}
#>
EMACS      ?= emacs
CASK       ?= {{scripts-dir}}/lcask

PROJECT      := {{name}}
INDEX	     := {{index}}
SRC_DIR      := {{src-dir}}
SRC_INDEX_EL := $(SRC_DIR)/$(INDEX_EL)
META_EL      := $(SRC_DIR)/$(INDEX)/META.el
SUBMOD_DIR   := $(SRC_DIR)/$(INDEX)
EMACS_OPTS   := --batch -Q -l {{scripts-dir}}/exec.el

INDEX_EL      := $(INDEX).el
TARGET	      := $(INDEX).elc
INDEX_BUNDLED := $(SRC_DIR)/$(INDEX).bundled.el

################################################################################
.DEFAULT_GOAL := help
#{{(@ENV)(lelde/tinplate/util::make-phoeny-macro @ENV)}}

emacs_common = $(EMACS) $(EMACS_OPTS) -L $(SRC_DIR)
emacs_integ  = $(EMACS) $(EMACS_OPTS) -L .
lelde_update = $(emacs_common) -l lelde -f lelde-update-project-files
lelde_fill   = $(emacs_common) -l lelde -f lelde-tinplate-fill
lelde_bundle = $(emacs_common) -l lelde -f lelde-elconc-bundle
lelde_stmax  = $(emacs_common) -l lelde -f lelde-stmax-file

# You can modify by custom.mk
-include custom.mk

.PHONY: $(PHONY)


################################################################################
#>This Makefile has following .PHONY tasks.
#>
#>help
#>    Show this message.
#>
help:
	@grep -e '^#>' Makefile |sed -e 's/^#>//'


################################################################################
#>all
#>    synonym to 'package'.
#>

all: package


################################################################################
#>package
#>    Build package to be able to distribute.
#>

.cask: Cask
	make clean-cask
	$(CASK) install

$(INDEX_EL): $(INDEX_BUNDLED)

$(INDEX_BUNDLED): $(SRC_DIR)/$(INDEX).el .cask
	$(lelde_bundle) $< $@

%.el: %.src.el .cask
	$(lelde_stmax) $<

%.elc: %.el .cask
	$(emacs_common) --eval "(setq byte-compile-error-on-warn t)"\
		-f batch-byte-compile $<

package:
	make update
	make $(INDEX_EL)

################################################################################
#>build
#>    Build the {{index}}.elc.
#>

build: package
	make $(TARGET)


################################################################################
#>update
#>    Update project files according to Lelde file
#>
#{{(@ENV) (lelde/tinplate/util::update-tasks @ENV)}}


################################################################################
#>clean
#>    Remove built files.
#>
clean:
	test -f $(TARGET) && rm $(TARGET) || true
	test -f $(INDEX_EL) && rm $(INDEX_EL) || true
	test -f $(SRC_DIR)/$(INDEX_EL) && rm $(SRC_DIR)/$(INDEX_EL) || true
	test -f $(INDEX_BUNDLED) && rm $(INDEX_BUNDLED) || true

#>clean-all
#>    Remove all generated files.
#>
clean-all: clean clean-cask

#>clean-cask
#>    Remove .cask directory.
#>
clean-cask:
	if test -d .cask; then rm -rf .cask; fi


################################################################################
#>test
#>    Run tests.
#>    If you want to run specific test script,
#>    you can use T environment variable. for example:
#>
#>        T={{test-scripts-dir}}/{{index}}.test.el make test
#>

T          ?=
I	   ?= $(shell find {{test-scripts-dir}} -name '*{{test-integration-suffix}}')
U	   ?= $(shell find {{test-scripts-dir}} -name '*{{test-unit-suffix}}')
TCOLS	   ?= 320
LI	   := $(foreach f,$(I),-l $f)
LU	   := $(foreach f,$(U),-l $f)

test_common := $(emacs_common) -l ert {{test-feature}}
test_integ  := $(emacs_integ)  -l ert {{test-feature}}
test_runner :=  {{test-runner}} -f ert-run-tests-batch-and-exit
test_truncate := TCOLS=$(TCOLS) {{scripts-dir}}/truncate

test:
	@{{scripts-dir}}/test $(T)

test-unit:
	@$(test_common) $(LU) $(test_runner) | $(test_truncate)

test-integration: $(INDEX_EL)
	@$(test_integ) $(LI) $(test_runner) | $(test_truncate)
