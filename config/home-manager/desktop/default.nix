{ pkgs, lib, config, ... }:
{
  imports = [
    ./btop.nix            # System Monitor
    ./fish.nix            # Shell
    ./fzf.nix             # Fuzzy search
    ./direnv.nix          # Automate dev environment when we enter directories
    ./qt.nix              # Setup theming for some set of apps
    ./gtk.nix             # Setup theming for some set of apps
    ./firefox             # Browser
    ./zathura.nix         # Documents
    ./git.nix
    ./mpv.nix             # Videos
    ./imv.nix             # Images
    ./beets.nix           # Music library manager
    ./foot.nix            # Terminal (very fast to open and no GPU acceleration)
    ./ghostty.nix         # Terminal (slower to open and has GPU acceleration)
    ./mangohud.nix        # Game HUD
    ./retroarch.nix       # Emulation
    ./discord.nix         # Social
    ./rofi.nix            # Alternative customizable menu
    ./lang-scala.nix      # Programming language
    ./heroic.nix          # Unified game client
    ./umu-launcher.nix    # Ad-hoc game launcher
    ./mpd.nix             # Music player
    ./wallpaper.nix       # Set wallpaper
    ./obsidian.nix        # note taking
    ./yazi.nix            # File browser
    ./helix.nix           # Editor
  ];

  xdg.mimeApps.enable = pkgs.stdenv.isLinux; # Default apps and directories

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
    includes = [ "$HOME/.ssh/config.local" ];
    matchBlocks = {
      "*".extraOptions.SetEnv = "TERM=xterm-256color";  # Sane default across different terminals. Don't need more.
      "bruno-home-nas" = {
        user = "Bruno-Admin";
        port = 6188;
      };
      "pi-zero".user = "pi";
      "rg353m".user = "ark";
      "pixel".user = "bruno";
    };
  };

  # Gpg
  programs.gpg = {
    enable = pkgs.stdenv.isLinux;
    homedir = "${config.xdg.dataHome}/gnupg";
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
    # Dev tools
    pkgs.parallel
    pkgs.yq-go          # Query YAML
    pkgs.xdg-user-dirs  # Relevant for desktop
    pkgs.dateutils
    pkgs.openssl

    # Archive
    pkgs.p7zip     # 7zip for linux
    pkgs.unrar     # Still need it

    # GUI centric
    pkgs.xwayland-satellite
    pkgs.qbittorrent   # Torrent client. FIXME: alternative TUI?
    pkgs.gparted       # TODO: I partially know how to do in the terminal... but... this is easier.
    pkgs.jetbrains.idea-oss
    pkgs.amp-cli       # AI Assistant.
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

  # Defensive hardening and to keep it explicit
  systemd.user.tmpfiles.rules = lib.optionals pkgs.stdenv.isLinux [
    "z ${config.programs.gpg.homedir}       0700 ${config.home.username} users"
  ];
}
