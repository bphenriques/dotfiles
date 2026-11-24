{ nixpkgs, generators, builders, ... }:

let
  inherit (generators) forAllSystems forLinuxSystems mergeAllSystems;

  crossPlatform = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      b = builders.${system};
    in rec {
      alfredo = pkgs.callPackage ./cli/alfredo { };
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
      awww-util = pkgs.callPackage ./desktop/awww-util { };
      screen-recorder = pkgs.callPackage ./desktop/screen-recorder { };
      screenshot = pkgs.callPackage ./desktop/screenshot { };
      mpc-plus = pkgs.callPackage ./desktop/mpc-plus { };
      displayctl = pkgs.callPackage ./desktop/displayctl { };
      calendar-cli = pkgs.callPackage ./desktop/calendar-cli { };
      handheld-sync = pkgs.callPackage ./devices/handheld-sync { inherit (b) writeNushellScript; };
    }
  );
in mergeAllSystems [ crossPlatform linux ]
