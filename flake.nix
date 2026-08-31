{
  description = "Plain, explicit JSON encode/decode functions for PureScript";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";

    al-dente.url = "git+ssh://git@github.com/m-bock/al-dente";
  };

  outputs = { self, nixpkgs, flake-utils, al-dente, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = al-dente.lib.${system};

        workspace = lib.mkWorkspace {
          src = ./.;
          name = "encode-decode";
          gitHashes.lint-purs = "sha256-CcbMkCKsPZwoGJLylQOspF+oOWgylkhcM7W5/7VGQcg=";
        };
      in
      {
        packages.default = workspace.output;

        # What the editor runs, so it never reaches for a globally
        # installed compiler or the one under node_modules.
        packages.toolchain = pkgs.symlinkJoin {
          name = "toolchain";
          paths = [ lib.defaults.purs lib.defaults.spago lib.defaults.purs-tidy ];
        };

        checks = {
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
