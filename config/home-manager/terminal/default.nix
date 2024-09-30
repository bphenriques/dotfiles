{ pkgs, lib, self, ... }:
{
  imports = [
    ./fish.nix
    ./fzf.nix
    ./broot.nix ./yazi.nix          # TODO: Decide one or another
    ./ghostty.nix
  ];

  programs.man = {
    enable = true;                  # I want manual pages
    generateCaches = true;          # Required for man completions. They are built during the build.
  };

  programs.ripgrep = {
    enable = true;
    arguments = [
      "--max-columns=150"
      "--max-columns-preview"
      "--glob=!.git"
      "--smart-case"
    ];
  };

  programs.tealdeer = {
    enable = true;
    settings = {
      display = {
        compact = false;
        use_pager = true;
      };

      updates.auto_update = false;
    };
  };

  home = {
    packages = with pkgs; [
      # Security
      openssl     # Generate keys with openssl rand -hex 32

      # Search
      fd          # A better `find`.

      # Text Processors
      jq          # Query JSON.
      yq-go       # Query YAML.
      vim         # Basic editor

      # Web
      wget        # Download stuff.

      # Monitoring
      procs       # Fancy `ps`.
      htop        # Fancy `top`.

      # Custom packages
      self.pkgs.frg         # Ripgrep + FZF
      self.pkgs.ffd         # FD + FZF to search nested directories
    ];
    sessionVariables = {
    #  Default editors and settings
      EDITOR  = "${pkgs.helix}/bin/hx";
      VISUAL  = "$EDITOR";
      PAGER   = "less -iMR";

      # Colors
      CLICOLOR  = 1;                                           # Enable ls colors in MacOS.
      LS_COLORS ="$(${pkgs.vivid}/bin/vivid generate snazzy)"; # LS_COLORS generator because I refuse to maintain one >.<
    };

    shellAliases = {
      # Default colorizatio
      diff = "diff --color=auto";
      grep = "grep --color=auto";
      egrep = "egrep --color=auto";
      fgrep = "fgrep --color=auto";
      ls = "ls --color=auto";

      # The usual aliases
      l = "ls -alh";
      ll = "ls -l";

      # Quality of life
      mkdir = "mkdir -pv";
      ".."  = "cd ..";
      "..."  = "cd ../..";
      ":q"   = "exit";
      tmpdir = "cd (mktemp -d)";

      # Text Processor
      e = "$EDITOR";

      # Utility
      whatsmyip = "curl ifconfig.me";
    } // (
      if pkgs.stdenv.isLinux then {
        pbcopy = lib.getExe pkgs.xclip;
        pbpaste = "${lib.getExe pkgs.xclip} -o";
      } else { }
    );
  };
}
