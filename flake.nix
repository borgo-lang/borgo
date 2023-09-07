{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
  };

  outputs = {
    self,
    nixpkgs,
    rust-overlay,
  }: let
    # TODO what's the correct incantation here?
    system = "x86_64-linux";

    pkgs = import nixpkgs {
      inherit system;
      overlays = [rust-overlay.overlays.default];
    };

    toolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;

  in {
    devShells.${system}.default = pkgs.mkShell {
      packages = [
        toolchain
        pkgs.go_1_19
        pkgs.just
        pkgs.deno
        pkgs.gofumpt
      ];
    };
  };
}

