export PATH := justfile_directory() / "node_modules/.bin:" + env_var('PATH')
set shell := ["bash", "-c"]

build:
    spago build

test:
    spago test

lint:
    spago test -m Test.Lint

format:
    purs-tidy format-in-place 'src/**/*.purs' 'test/**/*.purs'
