{ pkgs, lib, config, ... }:
{
  imports = [
    ./fonts.nix
    ./git.nix
    ./fish.nix        # Shell
    ./helix.nix       # Editor
    ./direnv.nix      # Automate dev environment when we enter directories
    ./fzf.nix         # Fuzzy search
    ./ripgrep.nix     # Search
    ./yazi.nix        # File browser
    ./lang-scala.nix  # Programming language
  ];

  # XDG Compliance to tidy up $HOME.
  xdg.enable = true;
  xdg.mimeApps.enable = pkgs.stdenv.isLinux; # Default apps and directories
  home.preferXdgDirectories = true;

  programs.bat.enable = true;             # Better file previewer
  programs.fd.enable = true;              # Better `find`.
  programs.jq.enable = true;              # JSON query.
  programs.htop.enable = true;            # Fancy `top`.
  custom.programs.project.enable = true;  # Easier way to navigate jump through different projects

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

  home.packages = with pkgs; [
    # Consistency across different operating systems
    coreutils
    findutils
    gnugrep
    watch
    tree
    parallel
    gnused
    dateutils
    unzip
    openssl
    xdg-user-dirs

    # Text Processors
    yq-go       # Query YAML.
    vim         # Basic editor

    # Archive
    p7zip     # 7zip for linux
    unrar     # Still need it

    # Monitoring
    procs       # Fancy `ps`.
  ];

  # Gpg
  programs.gpg.enable = pkgs.stdenv.isLinux;
  services.gpg-agent = {
    enable = pkgs.stdenv.isLinux;
    pinentryPackage = pkgs.pinentry-gnome3;
  };

  home = {
    sessionVariables = {
      VISUAL  = "$EDITOR";    # Set within the editor config.
      PAGER   = "less -iMR";

      # Colors
      LS_COLORS ="$(${pkgs.vivid}/bin/vivid generate snazzy)"; # LS_COLORS generator because I refuse to maintain one >.<
      LANG    = "en_US.UTF-8";
      LC_ALL  = "en_US.UTF-8";
    } // (lib.optionalAttrs pkgs.config.allowUnfree {
       NIXPKGS_ALLOW_UNFREE = 1;
    }) // (lib.optionalAttrs pkgs.stdenv.isDarwin {
      CLICOLOR  = 1;  # Enable ls colors in MacOS.
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

      # Utility
      whatsmyip = "${lib.getExe pkgs.curl} ifconfig.me";
    } // (lib.optionalAttrs pkgs.stdenv.isLinux {
      webp_to_png = ''nix-shell -p libwebp -p parallel --command "parallel dwebp {} -o {.}.png ::: *.webp"'';
    });
  };

  programs.man.enable = true;     # RTFM
  manual.manpages.enable = false; # Discard home-manager configuration man pages.
  manual.json.enable = false;     # Discard home-manager configuration JSON docs.

  # Tighten permissions to private keys
  systemd.user.tmpfiles.rules = lib.optionals pkgs.stdenv.isLinux [
    "z ${config.home.homeDirectory}/.ssh    0700 ${config.home.username} users"
    "z ${config.home.homeDirectory}/.gnupg  0700 ${config.home.username} users"
  ];
}
