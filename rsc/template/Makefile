#>Makefile for the package {{name}}
#>
EMACS      ?= emacs
CASK       ?= {{scripts-dir}}/lcask

PROJECT      := {{name}}
INDEX	     := {{index}}
INDEX_EL     := $(INDEX).el
TARGET	     := $(INDEX).elc
SRC_DIR      := src
SRC_INDEX_EL := $(SRC_DIR)/$(INDEX_EL)
META_EL      := $(SRC_DIR)/$(INDEX)/META.el
SUBMOD_DIR   := $(SRC_DIR)/$(INDEX)
EMACS_OPTS   := --batch -Q

################################################################################
.DEFAULT_GOAL := help
#{{(@ENV)(lelde/project/update/util::make-phoeny-macro @ENV)}}

emacs_common = $(CASK) exec $(EMACS) $(EMACS_OPTS) -L $(SRC_DIR)
emacs_integ  = $(CASK) exec $(EMACS) $(EMACS_OPTS) -L $(SRC_DIR)
lelde_update = $(emacs_common) -l lelde -f lelde-update-project-files
lelde_fill   = $(emacs_common) -l lelde -f lelde-fill
lelde_bundle = $(emacs_common) -l lelde -f lelde-bundle

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
#>    synonym to 'build'.
#>

all: build


################################################################################
#>build
#>    Build the {{index}}.elc.
#>

src/$(INDEX).bundled.el: src/$(INDEX).el Lelde .cask
	$(lelde_bundle) $< $@

.cask: Cask
	make clean-cask
	$(CASK) install

%.el: %.src.el .cask
	$(emacs_common) -l smex -f smex-file

%.elc: %.el .cask
	$(emacs_common) --eval "(setq byte-compile-error-on-warn t)"\
		-f batch-byte-compile $<

build:
	make update
	make $(TARGET)


################################################################################
#>update
#>    Update project files according to Lelde file
#>
#{{(@ENV) (lelde/project/update/util::update-tasks @ENV)}}


################################################################################
#>clean
#>    Remove built files.
#>
clean:
	test -f "$(TARGET)" && rm $(TARGET)

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
test_runner :=  {{test-runner}} -f ert-run-tests-batch-and-exit
test_truncate := TCOLS=$(TCOLS) {{scripts-dir}}/truncate

test:
	@{{scripts-dir}}/test $(T)

test-unit:
	@$(test_common) $(LU) $(test_runner) | $(test_truncate)

test-integration:
	@$(test_common) $(IU) $(test_runner) | $(test_truncate)
