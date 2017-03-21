{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc801" }:
let libcef3 = with nixpkgs; callPackage cef3-raw/libcef3.nix {};
    hpkgs = nixpkgs.haskell.packages.${compiler}.override {
      overrides = self: super: {
        cef3-raw = self.callPackage cef3-raw/cef3-raw.nix { inherit libcef3; };
        cef3-simple = self.callPackage ./cef3-simple.nix { };
      };
    };
in hpkgs.cef3-simple
