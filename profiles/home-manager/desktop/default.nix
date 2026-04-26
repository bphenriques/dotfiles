{ pkgs, lib, config, ... }:
{
  imports = [
    ./stylix.nix          # Color scheme, fonts, icons, cursor
    ./xdg-userdirs.nix    # XDG user directories, screenshots, recordings
    ./btop.nix            # System Monitor
    ./fish.nix            # Shell
    ./fzf.nix             # Fuzzy search
    ./qt.nix              # Setup theming for some set of apps
    ./gtk.nix             # Setup theming for some set of apps
    ./firefox             # Browser
    ./zathura.nix         # Documents
    ./mpv.nix             # Videos
    ./imv.nix             # Images
    ./beets.nix           # Music library manager
    ./ghostty.nix         # Terminal
    ./discord.nix         # Social
    ./rofi.nix            # Alternative customizable menu
    ./kdenlive.nix        # Video editor
    ./qbittorrent.nix     # Torrent client
    ./mpd.nix             # Music player
    ./awww.nix            # Wallpaper daemon
    ./obsidian.nix        # note taking
    ./yazi.nix            # File browser
  ];

  xdg.mimeApps.enable = pkgs.stdenv.isLinux;    # Default apps and directories

  # Enable easier font management
  fonts.fontconfig.enable = true;
  stylix.targets.fontconfig.enable = true;

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

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    includes = [ "$HOME/.ssh/config.local" ];
    matchBlocks = {
      "*" = {
        extraOptions.SetEnv = "TERM=xterm-256color";  # Sane default across different terminals. Don't need more.
        addKeysToAgent = "4h";  # Cache the keys temporarily but expire after 4 hours.
      };
      "bruno-home-nas" = {
        user = "Bruno-Admin";
        port = 6188;
      };
      "pi-zero".user = "pi";
      "rg353m".user = "ark";
      "pixel".user = "bruno";
    };
  };

  services.gpg-agent = {
    enable = pkgs.stdenv.isLinux;
    pinentry.package = pkgs.pinentry-gnome3;
  };

  programs.ripgrep = {
    enable = true;
    arguments = [ "--max-columns=150" "--max-columns-preview" "--glob=!.git" "--smart-case" ];
  };

  programs.zoxide = {
    enable = true;
    options = [ "--cmd j" ];
  };

  stylix.targets.bat.enable = true;
  programs.bat.enable = true;             # Better file previewer
  programs.fd.enable = true;              # Better `find`.
  programs.jq.enable = true;              # JSON query.
  custom.programs.project.enable = true;  # Easier way to navigate jump through different projects
  custom.programs.fzf-fd.enable = true;   # Fuzzy fd
  custom.programs.fzf-rg.enable = true;   # Fuzzy ripgrep
  programs.nushell.enable = true;         # Adhoc shell for data processing

  home.packages = lib.optionals pkgs.stdenv.isLinux [
    pkgs.xdg-user-dirs  # Relevant for desktop

    # Archive
    pkgs.p7zip     # 7zip for linux
    pkgs.unrar     # Still need it

    # GUI centric
    pkgs.gparted
  ];

  home = {
    sessionVariables = {
      LS_COLORS = "$(${lib.getExe pkgs.vivid} generate snazzy)";
    } // lib.optionalAttrs pkgs.stdenv.isDarwin {
      CLICOLOR = 1;
    };

    shellAliases = {
      l = "${lib.getExe pkgs.eza} -alh";
    };
  };

}
