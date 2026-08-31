export PATH := justfile_directory() / "node_modules/.bin:" + env_var('PATH')
set shell := ["bash", "-c"]

build:
    spago build

test:
    spago test

lint:
    spago test -m Test.Lint

# The gate's build: warnings are errors, and the wipe is what makes the
# count real - purs does not re-report a warning for a module it did not
# recompile, so an incremental strict build can print zero while warnings
# genuinely exist. Same thing CI does from a fresh checkout.
strict:
    rm -rf output
    spago build --strict

format:
    purs-tidy format-in-place 'src/**/*.purs' 'test/**/*.purs'

check: strict test lint
