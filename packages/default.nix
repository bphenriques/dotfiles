{ nixpkgs, generators, ... }:

let
  inherit (generators) forAllSystems forLinuxSystems mergeAllSystems;

  crossPlatform = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      builders = import ../lib/builders.nix { lib = nixpkgs.lib; inherit pkgs; };
    in rec {
      preview = pkgs.callPackage ./cli/preview { };
      fzf-rg = pkgs.callPackage ./cli/fzf-rg { inherit builders; };
      fzf-fd = pkgs.callPackage ./cli/fzf-fd { inherit preview builders; };
      project = pkgs.callPackage ./cli/project { inherit preview builders; };
      bw-session = pkgs.callPackage ./dotfiles/bw-session { };
      dotfiles = pkgs.callPackage ./dotfiles/dotfiles { inherit builders; };
      dotfiles-secrets = pkgs.callPackage ./dotfiles/dotfiles-secrets { inherit bw-session; };
      inky-setup = pkgs.callPackage ../hosts/inky { };
    }
  );

  linux = forLinuxSystems (system:
    let pkgs = nixpkgs.legacyPackages.${system};
    in {
      volume-osd = pkgs.callPackage ./desktop/volume-osd { };
      brightness-osd = pkgs.callPackage ./desktop/brightness-osd { };
      upower-notify = pkgs.callPackage ./desktop/upower-notify { };
      niri-window-dmenu = pkgs.callPackage ./desktop/niri-window-dmenu { };
      niri-keyboard-layout = pkgs.callPackage ./desktop/niri-keyboard-layout { };
      awww-util = pkgs.callPackage ./desktop/awww-util { };
      screen-recorder = pkgs.callPackage ./desktop/screen-recorder { };
      screenshot = pkgs.callPackage ./desktop/screenshot { };
      mpc-plus = pkgs.callPackage ./desktop/mpc-plus { };
      wg-manage = pkgs.callPackage ./homelab/wg-manage { };
      rustic-manage = pkgs.callPackage ./homelab/rustic-manage { };
      pocket-id-manage = pkgs.callPackage ./homelab/pocket-id-manage { };
    }
  );
in mergeAllSystems [ crossPlatform linux ]
