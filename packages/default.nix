{ nixpkgs, generators, builders, ... }:

let
  inherit (generators) forAllSystems forLinuxSystems mergeAllSystems;

  crossPlatform = forAllSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      b = builders.${system};
    in rec {
      preview = pkgs.callPackage ./cli/preview { };
      fzf-rg = pkgs.callPackage ./cli/fzf-rg { builders = b; };
      fzf-fd = pkgs.callPackage ./cli/fzf-fd { inherit preview; builders = b; };
      project = pkgs.callPackage ./cli/project { inherit preview; builders = b; };
      bw-session = pkgs.callPackage ./dotfiles/bw-session { };
      dotfiles = pkgs.callPackage ./dotfiles/dotfiles { builders = b; fleetHostIPs = (import ../hosts/shared.nix).lan.hosts; };
      dotfiles-secrets = pkgs.callPackage ./dotfiles/dotfiles-secrets { inherit bw-session; };
      inky-setup = pkgs.callPackage ../hosts/inky { };
    }
  );

  linux = forLinuxSystems (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      b = builders.${system};
    in rec {
      volume-osd = pkgs.callPackage ./desktop/volume-osd { };
      brightness-osd = pkgs.callPackage ./desktop/brightness-osd { };
      upower-notify = pkgs.callPackage ./desktop/upower-notify { };
      niri-keyboard-layout = pkgs.callPackage ./desktop/niri-keyboard-layout { };
      awww-util = pkgs.callPackage ./desktop/awww-util { };
      screen-recorder = pkgs.callPackage ./desktop/screen-recorder { };
      screenshot = pkgs.callPackage ./desktop/screenshot { };
      status-glance = pkgs.callPackage ./desktop/status-glance { inherit upower-notify volume-osd niri-keyboard-layout; };
      mpc-plus = pkgs.callPackage ./desktop/mpc-plus { };
      generate-pegasus-metadata = pkgs.callPackage ./desktop/generate-pegasus-metadata { inherit (b) writeNushellScript; };
      scrape-roms = pkgs.callPackage ./cli/scrape-roms { };
      fin = pkgs.callPackage ./cli/fin { };
      wg-manage = pkgs.callPackage ./homelab/wg-manage { };
      rustic-manage = pkgs.callPackage ./homelab/rustic-manage { };
      pocket-id-manage = pkgs.callPackage ./homelab/pocket-id-manage { };
    }
  );
in mergeAllSystems [ crossPlatform linux ]
