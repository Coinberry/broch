{ system ? builtins.currentSystem, nixpkgs ? import ./nix/nixpkgs.nix }:
let
  config = { allowUnfree = true; };
  overlays = [ (self: super: {
    haskellPackages = super.haskellPackages.override {
      overrides = haskellNew: _: {
        broch = haskellNew.callPackage ./broch.nix {};
      };
    };})];
  pkgs = import nixpkgs { inherit config; inherit system; inherit overlays; };
  tools = with pkgs; [ cabal-install wget ];
  releaseVersion = p: pkgs.haskell.lib.overrideCabal (pkgs.haskell.lib.justStaticExecutables p) (_: { doCheck = false; });
  envWithCabalAndTools = p: p.env.overrideAttrs (old: { buildInputs = old.buildInputs ++ tools; });
in
  rec { haskdeps = map (drv: "${drv}") (pkgs.haskell.lib.getBuildInputs broch).haskellBuildInputs;
        broch = pkgs.haskellPackages.broch;
        brochDev = envWithCabalAndTools broch;
	brochExe = releaseVersion broch;
      }
