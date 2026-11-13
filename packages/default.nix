{ nixpkgs, generators, builders, ... }:

let
  inherit (generators) forAllSystems forLinuxSystems mergeAllSystems;

  crossPlatform = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      b = builders.${system};
    in rec {
      alfredo = pkgs.callPackage ./cli/alfredo { inherit (b) writeNushellScript; };
      preview = pkgs.callPackage ./cli/preview { };
      fzf-rg = pkgs.callPackage ./cli/fzf-rg { builders = b; };
      project = pkgs.callPackage ./cli/project { inherit preview; builders = b; };
      bw-session = pkgs.callPackage ./dotfiles/bw-session { };
      dotfiles = pkgs.callPackage ./dotfiles/dotfiles { builders = b; };
      dotfiles-secrets = pkgs.callPackage ./dotfiles/dotfiles-secrets { inherit bw-session; };
    }
  );

  linux = forLinuxSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      b = builders.${system};
    in rec {
      audioctl = pkgs.callPackage ./desktop/audioctl { };
      upower-notify = pkgs.callPackage ./desktop/upower-notify { };
      awww-util = pkgs.callPackage ./desktop/awww-util { };
      screen-recorder = pkgs.callPackage ./desktop/screen-recorder { };
      screenshot = pkgs.callPackage ./desktop/screenshot { };
      mpc-plus = pkgs.callPackage ./desktop/mpc-plus { };
      desktop-shell = pkgs.callPackage ./desktop/desktop-shell { };
      desktop-shell-preview = pkgs.callPackage ./desktop/desktop-shell/preview { };
      desktop-shell-calendar-sync = pkgs.callPackage ./desktop/desktop-shell/calendar-sync { inherit (b) writeNushellScript; };
      displayctl = pkgs.callPackage ./desktop/displayctl { };
      handheld-sync = pkgs.callPackage ./devices/handheld-sync { inherit (b) writeNushellScript; };
    }
  );
in mergeAllSystems [ crossPlatform linux ]
