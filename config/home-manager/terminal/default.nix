{ pkgs, config, lib, self, ... }:
{
  imports = [
    ./fish.nix
    ./ghostty.nix
    ./yazi.nix
  ];

  programs.bat.enable = true;         # Better file previewer
  programs.fd.enable = true;          # Better `find`.
  programs.jq.enable = true;          # JSON query.
  programs.htop.enable = true;        # Fancy `top`.
  programs.man.enable = true;         # RTFM
  custom.programs.project.enable = true; # Easier way to navigate jump through different projects
  custom.programs.fuzzy-fd.enable = true;  # Fuzzy fd
  custom.programs.frg.enable = true;  # Fuzzy rip-grep

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

  programs.zoxide = {
    enable = true;
    options = [ "--cmd j" ];
  };

  # Interesting for Android dev: https://git.belanyi.fr/ambroisie/nix-config/src/branch/main/modules/home/direnv/lib/android.sh
  programs.direnv = {
    enable                  = true; # Automatically load .envrc or .env.
    nix-direnv.enable       = true; # Faster direnv for nix environments.
    silent                  = true; # Disable verbose messages when entering a directory.
    config.whitelist.prefix = [ config.custom.dotfiles.directory ]; # Surpress prompt in my private dotfiles
  };

  # Fuzzy matching
  programs.fzf = {
    enable = true;
    defaultCommand = "${lib.getExe pkgs.fd} --type file --hidden --exclude=.git";
    enableFishIntegration = true;

    defaultOptions = [
      "--height='80%'"
      "--marker='* '"
      "--pointer='▶'"
      "--preview-window='right:60%'"
      "--bind='ctrl-p:toggle-preview'"
      "--bind='alt-a:select-all'"
      "--bind='alt-n:deselect-all'"
      "--bind='ctrl-f:jump'"
    ];
  };

  home = {
    packages = with pkgs; [
      # Security
      openssl     # Generate keys with openssl rand -hex 32

      # Text Processors
      yq-go       # Query YAML.
      vim         # Basic editor

      # Monitoring
      procs       # Fancy `ps`.
    ];

    sessionVariables = {
      VISUAL  = "$EDITOR";    # Set within the editor config.
      PAGER   = "less -iMR";

      # Colors
      CLICOLOR  = 1;                                           # Enable ls colors in MacOS.
      LS_COLORS ="$(${pkgs.vivid}/bin/vivid generate snazzy)"; # LS_COLORS generator because I refuse to maintain one >.<
    } // (lib.optionalAttrs pkgs.config.allowUnfree {
      NIXPKGS_ALLOW_UNFREE = 1;
    });

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
      mkdir   = "mkdir -pv";
      ".."    = "cd ..";
      "..."   = "cd ../..";
      ":q"    = "exit";
      tmpdir  = "cd (mktemp -d)";

      # Text Processor
      e = "$EDITOR";

      # Nix utility functions to set the SHELL automatically
      nix-shell = "nix-shell --run $SHELL";
      devshell  = "nix develop --command $SHELL";
      whatsmyip = "${lib.getExe pkgs.curl} ifconfig.me";
    } // (lib.optionalAttrs pkgs.stdenv.isLinux {
        pbcopy = lib.getExe pkgs.xclip;
        pbpaste = "${lib.getExe pkgs.xclip} -o";
      }
    );
  };
}
