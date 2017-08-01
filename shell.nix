{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let
  inherit (nixpkgs) pkgs;
  dontCheck = pkgs.haskell.lib.dontCheck;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  haskellPackages_ = haskellPackages.override {
    overrides = self: super: {
      criterion = dontCheck (self.callPackage ../_deps/criterion-1.2.nix {});
      microstache = dontCheck (self.callPackage ../_deps/microstache-1.nix {});
      statistics = dontCheck (self.callPackage ../_deps/statistics-0.14.0.2.nix {});
      base-orphans = dontCheck (self.callPackage ../_deps/base-orphans-0.6.nix {});
      time = dontCheck (self.callPackage ../_deps/time-1.8.0.2.nix {});
      unix = dontCheck (self.callPackage ../_deps/unix-2.7.2.2.nix {});
      directory = dontCheck (self.callPackage ../_deps/directory-1.3.1.1.nix {});
      process = dontCheck (self.callPackage ../_deps/process-1.6.1.0.nix {});
      aeson = dontCheck (self.callPackage ../_deps/aeson-1.2.1.0.nix {});
    };
  };

  drv = haskellPackages_.callPackage ./default.nix {};

in
  if pkgs.lib.inNixShell then drv.env else drv
