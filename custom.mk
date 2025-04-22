#PHONY := $(PHONY) your-additional-phonytasks

# following section is special to lelde's bootstrap development
# update_makefile_itself = no
lelde_init   = $(emacs_common) -l lelde/project/init\
			       -f lelde/project/init::init-project
lelde_update = $(emacs_common) -l lelde/project/update\
			       -f lelde/project/update::update-project-files
lelde_fill   = $(emacs_common) -l lelde/tinplate -f lelde/tinplate::tinplate-fill
lelde_bundle = $(emacs_common) =l lelde/elconc   -f lelde/elconc::elconc-bundle

init.sh: Lelde
	$(emacs_common) -l lelde/project/update\
		--eval '(lelde/project/update::update-file "$@" "bootstrap")'
