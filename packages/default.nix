{ nixpkgs, mylib }:

let
  inherit (mylib.builders) forAllSystems forLinuxSystems;

  crossPlatform = forAllSystems (system:
    let pkgs = nixpkgs.legacyPackages.${system};
    in rec {
      dotfiles = pkgs.callPackage ./dotfiles { };
      preview = pkgs.callPackage ./preview { };
      fzf-rg = pkgs.callPackage ./fzf-rg { };
      fzf-fd = pkgs.callPackage ./fzf-fd { inherit preview; };
      project = pkgs.callPackage ./project { inherit preview; };
      bw-session = pkgs.callPackage ./bw-session { };
    }
  );

  linux = forLinuxSystems (system:
    let pkgs = nixpkgs.legacyPackages.${system};
    in {
      volume-osd = pkgs.callPackage ./volume-osd { };
      brightness-osd = pkgs.callPackage ./brightness-osd { };
      niri-window-dmenu = pkgs.callPackage ./niri-window-dmenu { };
      swww-util = pkgs.callPackage ./swww-util { };
      niri-smart-paste = pkgs.callPackage ./niri-smart-paste { };
      session-dmenu = pkgs.callPackage ./session-dmenu { };

      # Community
      sway-audio-idle-inhibit = pkgs.callPackage ./sway-audio-idle-inhibit { };
    }
  );
in mylib.builders.mergeSystems [ crossPlatform linux ]