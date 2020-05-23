{ pkgs ? import <nixpkgs> {
  config = {
    allowUnfree = true;
  };
}
}:

with pkgs; with callPackage ../devel/nix-extra {};

let
  emacs = emacsWithConfig {
    config = ./emacs;

    quick = true;

    packages = ep: with ep; [
      # Theme
      doom-themes
      
      # Major modes
      nix-mode haskell-mode yaml-mode
      markdown-mode

      # Util
      magit deadgrep use-package hydra

      # Zettelkasten
      deft zetteldeft avy ace-window
    ];
  };
in userEnv {
  static = false;

  packages = [
    (hiPrio emacs)
    
    steam lutris git flameshot
    transmission-gtk
  ];
}
