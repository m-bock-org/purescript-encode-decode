{
  description = "Plain, explicit JSON encode/decode functions for PureScript";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    al-dente.url = "git+ssh://git@github.com/m-bock-org/al-dente";

    # The linter, as an input rather than something fetched while a
    # recipe runs. A check is a derivation and a derivation has no
    # network, so the only way it can lint is with the linter already
    # in its closure - and flake.lock is then what pins the rule set,
    # which is a better pin than a rev pasted into a justfile.
    lint-regulator.url = "git+ssh://git@github.com/m-bock-org/purescript-lint-regulator";
  };

  outputs = { self, nixpkgs, flake-utils, al-dente, lint-regulator, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = al-dente.lib.${system};

        # What the editor runs, so it never reaches for a globally
        # installed compiler or the one under node_modules.
        toolchain = lib.toolchain;

        workspace = lib.mkWorkspace {
          src = ./.;
          name = "encode-decode";
        };
      in
      {
        packages.default = workspace.output;

        # What the editor runs, so it never reaches for a globally
        # installed compiler or the one under node_modules.
        packages.toolchain = toolchain;

        # The linter this repository is held to, re-exported so there is
        # one pin for it and not two. `just lint` and the `lint` check
        # are then the same binary by construction, and flake.lock is
        # the only place its version is written down.
        packages.lint = lint-regulator.packages.${system}.lint-public;
        # The compiled test closure - every dependency plus the local
        # packages built with their tests. `just output` copies this so a
        # dev shell never recompiles what al-dente already built once per
        # machine, which is the whole point of building with al-dente.
        packages.testOutput = workspace.testOutput;

        # Makes `output/` match the built closure, and does nothing when
        # it already does - so a build can depend on it unconditionally.
        #
        # The hand-rolled version this replaces guarded on "output/ is
        # missing", which al-dente's own note calls out as the wrong
        # guard: bumping a dependency leaves a directory that exists and
        # is stale, and spago answers that by compiling every dependency
        # from source. That is what turned a one-line pin change into a
        # 541-module rebuild, and it is the granularity this workspace
        # is built to have.
        packages.restoreOutput = lib.mkRestore { output = workspace.testOutput; };

        checks = {
          # The public style, as a derivation. `just lint` runs the same
          # binary against your working tree; this runs it against what
          # is committed, which is what a pull request is.
          #
          # It needs spago and a populated `.spago`, because the linter
          # finds the workspace by asking `spago ls packages --json`
          # rather than by reading spago.yaml itself. Both come from
          # al-dente, so this costs no compile: the dependencies are
          # already built and the `.spago` is the same store path the
          # build uses.
          lint = pkgs.runCommand "lint"
            {
              nativeBuildInputs = [ lib.defaults.spago lib.defaults.purs pkgs.nodejs pkgs.git pkgs.jq ];
            } ''
            cp -a ${self} src
            chmod -R u+w src
            cd src
            cp -a ${workspace.dotSpago}/. .spago
            chmod -R u+w .spago
            export HOME="$TMPDIR/home"
            mkdir -p "$HOME/.cache/spago-nodejs"
            touch "$HOME/.cache/spago-nodejs/fresh-registry-canary.txt"
            ${lint-regulator.packages.${system}.lint-public}/bin/lint-public
            touch $out
          '';

          # The spec suite, run from the store.
          tests = pkgs.runCommand "encode-decode-tests" { } ''
            ${lib.mkRunner {
              name = "spec";
              mainModule = "Test.Main";
              output = workspace.testOutput;
            }}/bin/spec
            touch $out
          '';
        };

        devShells.default = pkgs.mkShell {
          name = "encode-decode";

          # A marker that you are inside the dev shell. `nix develop` used
          # to do this itself and stopped, and the difference matters:
          # outside it, `purs` is whatever is installed globally.
          shellHook = ''
            case $- in *i*) export PS1="(encode-decode) $PS1" ;; esac

            # Point the editor at this exact toolchain, refreshed on every
            # entry so it cannot go stale against the flake. The .vscode
            # wrappers read this symlink and then need no nix at all.
            ln -sfn ${toolchain} .vscode/.toolchain
          '';

          packages = [
            lib.defaults.purs
            lib.defaults.spago
            lib.defaults.nodejs
            lib.defaults.purs-tidy
            pkgs.just
            pkgs.git
          ];
        };
      });
}
