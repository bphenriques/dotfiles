{ pkgs, lib, config, ... }:
{
  imports = [
    ./btop.nix        # System Monitor
    ./direnv.nix      # Automate dev environment when we enter directories
    ./fish.nix        # Shell
    ./fzf.nix         # Fuzzy search
    ./git.nix
    ./helix.nix       # Editor
    ./lang-scala.nix  # Programming language
    ./yazi.nix        # File browser
  ];

  # XDG Compliance to tidy up $HOME.
  xdg.enable = true;
  xdg.mimeApps.enable = pkgs.stdenv.isLinux; # Default apps and directories
  home.preferXdgDirectories = true;

  # Enable easier font management
  fonts.fontconfig.enable = true;

  stylix.targets.bat.enable = true;
  programs.bat.enable = true;             # Better file previewer
  programs.fd.enable = true;              # Better `find`.
  programs.jq.enable = true;              # JSON query.
  custom.programs.project.enable = true;  # Easier way to navigate jump through different projects
  custom.programs.fzf-fd.enable = true;   # Fuzzy fd
  custom.programs.fzf-rg.enable = true;   # Fuzzy ripgrep

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

  programs.ripgrep = {
    enable = true;
    arguments = [ "--max-columns=150" "--max-columns-preview" "--glob=!.git" "--smart-case" ];
  };

  programs.zoxide = {
    enable = true;
    options = [ "--cmd j" ];
  };

  home.packages = [
    # Consistency across different operating systems
    pkgs.coreutils
    pkgs.findutils
    pkgs.gnugrep
    pkgs.watch
    pkgs.tree
    pkgs.parallel
    pkgs.gnused
    pkgs.dateutils
    pkgs.unzip
    pkgs.openssl
    pkgs.xdg-user-dirs
    pkgs.file

    # Text Processors
    pkgs.yq-go       # Query YAML.
    pkgs.vim         # Basic editor

    # Archive
    pkgs.p7zip     # 7zip for linux
    pkgs.unrar     # Still need it
  ];

  # Gpg
  programs.gpg = {
    enable = pkgs.stdenv.isLinux;
    homedir = "${config.xdg.dataHome}/gnupg";
  };
  services.gpg-agent = {
    enable = pkgs.stdenv.isLinux;
    pinentryPackage = pkgs.pinentry-gnome3;
  };

  home = {
    sessionVariables = {
      VISUAL  = "$EDITOR";
      PAGER   = "less -iMR";

      # Colors
      LS_COLORS = "$(${lib.getExe pkgs.vivid} generate snazzy)"; # I refuse to maintain one >.<
      LANG    = "en_US.UTF-8";
      LC_ALL  = "en_US.UTF-8";
    } // (lib.optionalAttrs pkgs.config.allowUnfree {
       NIXPKGS_ALLOW_UNFREE = 1;
    }) // (lib.optionalAttrs pkgs.stdenv.isDarwin {
      CLICOLOR  = 1;
    });

    shellAliases = {
      # Default colorization
      diff = "diff --color=auto";
      grep = "grep --color=auto";
      egrep = "egrep --color=auto";
      fgrep = "fgrep --color=auto";
      ls = "ls --color=auto";

      # Quality of life
      l = "${lib.getExe pkgs.eza} -alh";
      mkdir   = "mkdir -pv";
      ".."    = "cd ..";
      "..."   = "cd ../..";
      ":q"    = "exit";
      tmpdir  = "cd (mktemp -d)";

      # Text Processor
      e = "$EDITOR";

      # Utility
      whatsmyip = "${lib.getExe pkgs.curl} ifconfig.me";
      webp_to_png = lib.mkIf pkgs.stdenv.isLinux ''nix-shell -p libwebp -p parallel --command "parallel dwebp {} -o {.}.png ::: *.webp"'';
    };
  };

  programs.ssh = {
    enable = true;
    includes = [ "$HOME/.ssh/config.local" ];
    matchBlocks = {
      "*".extraOptions.SetEnv = "TERM=xterm-256color";  # Sane default across different terminals. Don't need more.
      "bruno-home-nas" = {
        user = "Bruno-Admin";
        port = 6188;
      };
      "pi-zero".user = "pi";
      "rg353m".user = "ark";
    };
  };

  # Discard home-manager configuration manual.
  manual = {
    html.enable = false;
    manpages.enable = false;
    json.enable = false;
  };

  # Tighten permissions to private keys
  systemd.user.tmpfiles.rules = lib.optionals pkgs.stdenv.isLinux [
    "z ${config.home.homeDirectory}/.ssh    0700 ${config.home.username} users"
    "z ${config.home.homeDirectory}/.gnupg  0700 ${config.home.username} users"
  ];
}
