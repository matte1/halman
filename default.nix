{ pkgs ? import ./nixpkgs.nix {} }:
with pkgs;
rec {
  inherit pkgs;

  # Explicit list of used files. Else there is always too much and
  # cache is invalidated.
  sources = lib.sourceByRegex ./.  [
    "halman\.cabal$"
    ".*\.hs$"
    "src"
    "src/Math/*"
    "src/Models/*"
    "src/Sim/*"
    "src/Physics/*"
    "app"
    "src/*"
    "LICENSE"
  ];

  halmanBuilder = hPkgs: (haskell.lib.buildFromSdist (hPkgs.callCabal2nix "halman" sources {})).overrideAttrs(
    oldAttrs: {
      buildInputs = oldAttrs.buildInputs ++ [python3];
    });

  halman_86 = halmanBuilder haskell.packages.ghc865;
  halman_88 = halmanBuilder (haskell.packages.ghc884.override {
    overrides = self: super: with hasell.lib; {
      megaparsec = super.megaparsec_9_0_0;
    };
  });
  halman_810 = halmanBuilder (haskell.packages.ghc8102.override {
    overrides = self: super: with haskell.lib; {
      th-expand-syns = doJailbreak super.th-expand-syns;
    };
  });

  halman = halman_88;

  # Run hlint on the codebase
  hlint = runCommand "hlint-krank" {
    nativeBuildInputs = [haskellPackages.hlint];
  }
  ''
  cd ${sources}
  hlint .
  mkdir $out
  '';

  # Run ormolu on the codebase
  # Fails if there is something to format
  ormolu = runCommand "ormolu-krank" {
    nativeBuildInputs = [haskellPackages.ormolu];
  }
  ''
  cd ${sources}
  ormolu --mode check $(find -name '*.hs')
  mkdir $out
  '';

  hlint-fix = mkShell {
    nativeBuildInputs = [haskellPackages.hlint git haskellPackages.apply-refact];
    shellHook = ''
      for file in $(git ls-files | grep '\.hs$')
      do
        hlint  --refactor --refactor-options='-i' $file
      done
      exit 0
    '';
  };

  ormolu-fix = mkShell {
    nativeBuildInputs = [haskellPackages.ormolu git];
    shellHook = ''
      ormolu --mode inplace $(git ls-files | grep '\.hs$')
      exit 0
    '';
  };
}
