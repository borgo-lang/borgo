{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
    rust-overlay,
  }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
    	  inherit system;
    	  overlays = [rust-overlay.overlays.default];
    	};

    	toolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;

      in {
        devShells.default = pkgs.mkShell {
          packages = [
            toolchain
            pkgs.go_1_19
            pkgs.just
            pkgs.deno
            pkgs.gofumpt
          ];
        };
      }
    );
}

