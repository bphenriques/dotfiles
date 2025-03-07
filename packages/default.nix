{ nixpkgs, mylib }:

let
  inherit (mylib.generators) forAllSystems forLinuxSystems mergeAllSystems;

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
      niri-keyboard-layout = pkgs.callPackage ./niri-keyboard-layout { };
      niri-smart-paste = pkgs.callPackage ./niri-smart-paste { };

      powerprofilesctl-notify = pkgs.callPackage ./powerprofilesctl-notify { };
      swww-util = pkgs.callPackage ./swww-util { };
      screen-recorder = pkgs.callPackage ./screen-recorder { };
      screenshot = pkgs.callPackage ./screenshot { };
    }
  );
in mergeAllSystems [ crossPlatform linux ]