{
  description = "compile ghc using dyn drvs";
  inputs = {
    nixpkgs-for-tests-and-lib-src.url = "github:NixOS/nixpkgs";
    nixpkgs-for-tests-and-lib-src.flake = false;
    procex-for-tests-src.url = "github:L-as/procex";
    procex-for-tests-src.flake = false;
  };
  outputs = { nixpkgs-for-tests-and-lib-src, procex-for-tests-src, self }:
    let
      undefined = builtins.throw "undefined";
      lib = import "${nixpkgs-for-tests-and-lib-src}/lib";
      systems = [ "x86_64-linux" ];
      procex-for-tests = (import "${procex-for-tests-src}/flake.nix").outputs
        { self = procex-for-tests; nixpkgs-for-tests-and-lib =undefined; };
      nixpkgs-for-tests-and-lib = lib.genAttrs systems (system: import nixpkgs-for-tests-and-lib-src {
        inherit system;
        config = {};
        overlays = [
          procex-for-tests.overlays.default
          (final: prev: { haskellPackages =
            prev.haskellPackages.extend procex-for-tests.hsOverlay;
          })
          self.overlays.default
        ];
      });
      ghc-src = builtins.derivation {
        system = "builtin";
        builder = "builtin:fetchurl";
        outputHashMode = "flat";
        outputHashAlgo = "sha256";
        outputHash = "sha256-aKBL1ktK7bLbTQYhqdIE6MZUtSbgIfVFXk516C+4U+w=";
        name = "ghc-source.tar.gz";
        url = "https://github.com/ghc/ghc/archive/47646ce28319aca8b92582c00f4be37714ca2a2d.tar.gz";
        executable = false;
        unpack = false;
        preferLocalBuild = true;
      };
    in {
      overlays = {
        default = final: prev:
          let ghc = final.haskellPackages.ghcWithPackages (o: [ o.procex ]); in
          {
            dyn-drv-ghc = builtins.derivation {
              inherit (final) system;
              name = "dyn-drv-ghc";
              builder = "${ghc}/bin/runhaskell";
              args = [ ./Script.hs ];
              requiredSystemFeatures = [ "recursive-nix" ];
              SRC = ghc-src;
            };
          };
      };
      packages = lib.genAttrs systems (system: {
        default = nixpkgs-for-tests-and-lib.${system}.dyn-drv-ghc;
      });
      # FIXME: Don't use Nixpkgs for shell
      devShells = lib.genAttrs systems (system:
        let pkgs = nixpkgs-for-tests-and-lib.${system}; in
        {
          default = pkgs.mkShell {
            nativeBuildInputs = [
              pkgs.cabal-install
              (pkgs.haskellPackages.ghcWithPackages (o: [ o.procex ]))
              pkgs.haskell-language-server
              pkgs.clang-tools
              pkgs.libprocex-glue
              pkgs.ormolu
            ];
          };
        }
      );
  };
}
