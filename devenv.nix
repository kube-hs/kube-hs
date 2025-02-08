{ pkgs, lib, config, inputs, ... }:

{
  packages = with pkgs.haskellPackages; [
    hoogle
    doctest
    haskell-dap
    cabal-gild
    Cabal_3_10_3_0
    haskell-language-server
    haskell-debug-adapter
  ];

  languages.haskell = {
    enable = true;
    package = pkgs.haskell.compiler.ghc984;
  };
}
