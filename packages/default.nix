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
    }
  );

  linux = forLinuxSystems (system:
    let pkgs = nixpkgs.legacyPackages.${system};
    in {
      volume-osd = pkgs.callPackage ./volume-osd { };
      brightness-osd = pkgs.callPackage ./brightness-osd { };
      niri-output-configuration = pkgs.callPackage ./niri-output-configuration { };
      niri-window-dmenu = pkgs.callPackage ./niri-window-dmenu { };
      swww-util = pkgs.callPackage ./swww-util { };
      cliphist-dmenu = pkgs.callPackage ./cliphist-dmenu { };
      smart-paste = pkgs.callPackage ./smart-paste { };
      session-dmenu = pkgs.callPackage ./session-dmenu { };
      ddcci-util = pkgs.callPackage ./ddcci-util { };

      # Move to community namespace
      proton-ge-custom = pkgs.callPackage ./proton-ge-custom { };
      sway-audio-idle-inhibit = pkgs.callPackage ./sway-audio-idle-inhibit { };
    }
  );
in forAllSystems (system:
  nixpkgs.lib.attrsets.mergeAttrsList [
    crossPlatform.${system}
    (linux.${system} or { })
  ]
)
