#! /usr/bin/bash

if ! command -v cask >/dev/null; then
    cat <<EOF >&2
Lelde requires Cask to be installed.
Please refer to the official installation guide for instructions:
https://cask.readthedocs.io/en/latest/guide/installation.html
EOF
    exit 1
fi

if ! command -v make >/dev/null; then
    cat <<EOF >&2
Lelde requires 'make' to be installed.
Please refer to your distribution's package manager or documentation for
installation instructions.
EOF
    exit 1
fi

if [ -f Cask ] ; then
    mv Cask Cask.bak
    echo "Cask is exists. it renamed as Cask.bak"
fi


cat <<EOF >Cask
(source gnu)
(source melpa)
(source "looper" "https://lieutar.github.io/looper-elpa/")
(depends-on "lelde")
EOF

cask install
cask exec emacs --batch -l lelde -f lelde-init-project
