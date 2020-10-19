let nixpkgs = fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/release-20.03.tar.gz";
      sha256 = "15fzr98ymm64j9wc7slcrlikzgxq3znrmn6mnkz5kks01s9fff8q";
    };
    emacs-overlay = import (fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    });
in
{ nix-extra ? fetchTarball {
  url = "https://github.com/lambdadog/nix-extra/archive/master.tar.gz";
}
, pkgs ? import nixpkgs {
  config = {
    allowUnfree = true;
  };
  overlays = [
    emacs-overlay
    ((import nixpkgs {}).callPackage nix-extra {}).overlay
  ];
}
}:

with pkgs;

let
  emacs = emacsWithConfig {
    config = ./emacs;

    quick = true;

    packages = ep: with ep; [
      # Theme
      doom-themes

      # QoL
      dashboard page-break-lines
      
      # Major modes
      nix-mode haskell-mode yaml-mode
      markdown-mode

      # Util
      magit deadgrep use-package hydra
      ryo-modal alert pinentry

      # Zettelkasten
      deft
    ];

    extraDependencies = [
      imagemagickBig ispell
    ];
  };
in {
  inherit emacs;
  user-env = userEnv {
    static = false;
    
    packages = [
      # Emacs config
      (hiPrio emacs)

      # GUI
      steam lutris flameshot
      transmission-gtk calibre xournal
      krita mpv obs-studio wasabiwallet
      zoom-us

      # CLI
      ffmpeg-full htop steam-run
      libstrangle p7zip unar ripgrep
      tree wget youtube-dl

      gitAndTools.gitFull

      # Haskell
      cabal-install
    ];
  };
}
