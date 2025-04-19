EMACS      ?= emacs
CASK       ?= scripts/lcask
T          ?= $(shell find tests -name '*.test.el')
TCOLS	   ?= 320

PROJECT      := {{name}}
INDEX	     := {{index}}
INDEX_EL     := $(INDEX).el
TARGET	     := $(INDEX).elc
PKG_EL       := $(INDEX)-pkg.el
LT           := $(foreach f,$(T),-l $f)
SRC_DIR      := src
SRC_INDEX_EL := $(SRC_DIR)/$(INDEX_EL)
META_EL      := $(SRC_DIR)/$(INDEX)/META.el
SUBMOD_DIR   := $(SRC_DIR)/$(INDEX)
EMACS_OPTS   := --batch -Q -L $(SRC_DIR)

################################################################################
.DEFAULT_GOAL := help
PHONY	     := help all build clean clean-all clean-cask test update

update_makefile_itself := yes

emacs_common = $(CASK) exec $(EMACS) $(EMACS_OPTS)
lelde_update = $(emacs_common) -l lelde -f lelde-update-project-files
lelde_fill   = $(emacs_common) -l lelde -f lelde-fill
lelde_bundle = $(emacs_common) -l lelde -f lelde-bundle

# You can modify by custom.mk
-include custom.mk

.PHONY: $(PHONY)


################################################################################
#>Makefile for {{name}}
#>
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
update :=

update := $(update) Cask
Cask: Lelde
	$(lelde_update) $@

ifeq ($(update_makefile_itself),yes)
update := $(update) Makefile
Makefile: Lelde .cask
	$(lelde_update) $@
endif

update := $(update) $(PKG_EL)
$(PKG_EL): Lelde .cask
	$(lelde_update) $@

update := $(update) $(META_EL)
$(META_EL): Lelde .cask
	$(lelde_update) $@

update := $(update) recipe/public/$(INDEX)
recipe/public/$(INDEX):
	$(lelde_update) $@

update := $(update) recipe/local/$(INDEX)
recipe/local/$(INDEX):
	$(lelde_update) $@

update := $(update) README.md
README.md: src/README.md Lelde .cask
	$(lelde_fill) $< $@

update := $(update) $(INDEX).el
$(INDEX).el: src/$(INDEX).bundled.el Lelde .cask
	$(lelde_fill) $< $@

update: $(update)


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
#>        T=scripts/{{index}}.test.el make test
#>
test:
	$(emacs_common) -l ert \
			{{test-feature}} \
			$(LT) \
			{{test-runner}} \
			-f ert-run-tests-batch-and-exit \
   |awk -ve="$INSIDE_EMACS" -e \
   '{if(e&&length($$0)>$(TCOLS)){print substr($$0,1,$(TCOLS))" ..."}else{print}}'
