# Deliberately NOT putting node_modules/.bin on PATH. package.json still
# carries purs/spago/purs-tidy because CI installs them with npm, but a
# recipe run inside `nix develop` must use the compiler the flake pins -
# node_modules/.bin first on PATH silently wins over it.
set shell := ["bash", "-c"]

# Restores output/ first if there is none - a fresh clone then compiles
# nothing, because al-dente already built every dependency. Only when it
# is missing: once you have edited anything, the store copy is behind
# your working tree and replacing output/ would throw away exactly the
# incremental state that makes a rebuild fast.
build:
    #!/usr/bin/env bash
    set -euo pipefail
    [ -d output ] || just output
    spago build

test:
    spago test

# The public style, run as a binary rather than as a dependency.
#
# It cannot be a dependency: the regulator depends on this package, so
# the arrow only goes one way. The binary has no such problem - it
# reads the workspace it is run in, and `lint-exemptions.json` beside
# this file is where any departure from the style goes.
#
# `--fix <command>` names a program that proposes fixes for findings the
# style has guidance for. What that program talks to is its own
# business; the linter judges what comes back.
# Pinned, and it has to be. Unpinned, this fetched whatever the linter's
# main happened to be when CI ran - so this repository could go red
# without anyone touching it, and the rule set moved three times in one
# afternoon. A gate that moves under you is not a gate.
#
# The pin lives in flake.lock now, not in a rev pasted here. This is the
# same binary the `lint` check runs, so what you see locally is what a
# pull request is held to.
lint *ARGS:
    nix run .#lint -- {{ARGS}}

format:
    purs-tidy format-in-place 'src/**/*.purs' 'test/**/*.purs'

check: test lint

# Restore output/ from the Nix build rather than compiling it here. A
# copy, not symlinks: purs writes into output/<Module>/ in place, and a
# read-only store symlink dies on the first local edit.
output:
    nix run .#restoreOutput

