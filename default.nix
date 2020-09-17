let nixpkgs = fetchTarball {
      url = "https://github.com/NixOS/nixpkgs/archive/release-20.03.tar.gz";
      sha256 = "1sgfyxi4wckivnbniwmg4l6n9v5z6v53c5467d7k7pr2h6nwssfn";
    };
    emacs-overlay = import (fetchTarball {
      url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    });
    personal-overlay = (import (fetchTarball {
      url = "https://github.com/lambdadog/nix-extra/archive/master.tar.gz";
    }) { pkgs = import nixpkgs {}; }).overlay;
in
{ pkgs ? import nixpkgs {
  config = {
    allowUnfree = true;
  };
  overlays = [
    emacs-overlay
    personal-overlay
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
  };
in {
  inherit emacs;
  user-env = userEnv {
    static = false;
    
    packages = [
      # Emacs config
      (hiPrio emacs) imagemagickBig

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
