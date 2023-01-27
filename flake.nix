{
  description = "The vunk programming languagr";
  inputs = {
    nixpkgs.url = "nixpkgs/nixos-22.11";
    flake-utils.url = "github:numtide/flake-utils";
    crane = {
      url = "github:ipetkov/crane";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        flake-utils.follows = "flake-utils";
      };
    };
  };

  outputs = { self, nixpkgs, crane, flake-utils, rust-overlay, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ (import rust-overlay) ];
        };

        rustTarget = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;

        craneLib = (crane.mkLib pkgs).overrideToolchain rustTarget;

        tomlInfo = craneLib.crateNameFromCargoToml { cargoToml = ./Cargo.toml; };
        inherit (tomlInfo) version;
        pname = "vunk";

        src =
          let
            markdownFilter = path: _type: pkgs.lib.hasSuffix ".md" path;
            filterPath = path: type: builtins.any (f: f path type) [
              markdownFilter
              craneLib.filterCargoSources
              pkgs.lib.cleanSourceFilter
            ];
          in
          pkgs.lib.cleanSourceWith {
            src = ./.;
            filter = filterPath;
          };

        cargoArtifacts = craneLib.buildDepsOnly {
          inherit src pname;

          buildInputs = [
            pkgs.pkg-config
          ];
        };

        vunk = craneLib.buildPackage {
          inherit cargoArtifacts src pname version;

          cargoExtraArgs = "--all-features";

          buildInputs = [
            pkgs.pkg-config
          ];
        };
      in
      rec {
        checks = {
          inherit vunk;

          vunk-clippy = craneLib.cargoClippy {
            inherit cargoArtifacts src pname;
            cargoClippyExtraArgs = "--benches --examples --tests --all-features -- --deny warnings";
          };

          vunk-fmt = craneLib.cargoFmt {
            inherit src pname;
          };
        };

        packages.vunk = vunk;
        packages.default = packages.vunk;

        apps.vunk = flake-utils.lib.mkApp {
          name = "vunk";
          drv = vunk;
        };
        apps.default = apps.vunk;

        devShells.default = devShells.vunk;
        devShells.vunk = pkgs.mkShell {
          buildInputs = [
            pkgs.pkg-config
          ];

          nativeBuildInputs = [
            rustTarget

            pkgs.cargo-deny
            pkgs.gitlint
          ];
        };
      }
    );
}
