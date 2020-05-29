#!/usr/bin/env bash

# This file is auto-generated by Emacs via `(org-babel-tangle-file "configuration.org")'.

set -euxo pipefail

rustup component add rustfmt

curl https://github.com/snoe/clojure-lsp/releases/download/release-20200514T134144/clojure-lsp \
     -L  \
     -o /usr/local/bin/clojure-lsp
chmod 755 /usr/local/bin/clojure-lsp

python3 -m pip install mypy flake8 pylint black
