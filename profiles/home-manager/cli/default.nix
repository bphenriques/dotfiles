{ pkgs, lib, osConfig, self, config, ... }:
let
  guestBlocks = lib.concatMapAttrs (vmHost: guests:
    lib.mapAttrs (_: ip: {
      HostName = ip;
      ProxyJump = vmHost;
      StrictHostKeyChecking = "accept-new";
    }) guests
  ) osConfig.fleet.microvms;
in
{
  imports = [
    ./zsh.nix             # POSIX-ish shell
    ./fzf.nix             # Fuzzy search
    ./btop.nix            # System Monitor
    ./yazi.nix            # File browser
  ];

  programs.nushell.enable = true;               # Adhoc shell for data processing
  programs.nushell.loadSessionVariables = true;
  programs.bash.enable = true;                  # Login/ssh shell; without it home.sessionVariables reach zsh only

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    includes = [ "$HOME/.ssh/config.local" ];
    settings = {
      "*" = {
        SetEnv.TERM = "xterm-256color";   # Sane default across different terminals. Don't need more.
        AddKeysToAgent = "8h";            # Cache the keys temporarily but expire after a working day.
      };

      # Per host
      "pi-zero".User = "pi";
      "rg353m".User = "ark";
      "pixel".User = "bruno";
      "compute".StrictHostKeyChecking = "accept-new"; # one-time accept on first connection.
    } // guestBlocks;
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

  services.gpg-agent = {
    enable = true;
    pinentry.package = if pkgs.stdenv.hostPlatform.isDarwin then pkgs.pinentry_mac else pkgs.pinentry-gnome3;
  };

  # home-manager#5801: launchctl setenv for home.sessionVariables (macOS has no environment.d).
  launchd.agents.launchctl-setenv = lib.mkIf pkgs.stdenv.hostPlatform.isDarwin (
    let
      launchctl-setenv = pkgs.writeShellApplication {
        name = "launchctl-setenv";
        text = lib.concatStringsSep "\n" (lib.mapAttrsToList
          (name: val:
            "/bin/launchctl setenv ${lib.escapeShellArg name} ${lib.escapeShellArg (toString val)}")
          config.home.sessionVariables);
      };
    in
    {
      enable = true;
      config = {
        ProgramArguments = [ (lib.getExe launchctl-setenv) ];
        KeepAlive.SuccessfulExit = false;
        RunAtLoad = true;
      };
    }
  );

  programs.ripgrep = {
    enable = true;
    arguments = [ "--max-columns=150" "--max-columns-preview" "--glob=!.git" "--smart-case" ];
  };

  programs.zoxide = {
    enable = true;
    options = [ "--cmd j" ];
  };

  programs.vivid.enable = true;           # `LS_COLORS` generator
  programs.vivid.activeTheme = "one-dark";

  programs.bat.enable = true;             # Better file previewer
  stylix.targets.bat.enable = true;

  programs.fd.enable = true;              # Better `find`.
  programs.jq.enable = true;              # JSON query.

  home.packages = [
    pkgs.dateutils
    pkgs.openssl
    pkgs.yq-go             # Query YAML
    self.packages.fzf-rg   # Fuzzy ripgrep
    self.packages.project  # Jump between projects
  ] ++ lib.optionals pkgs.stdenv.hostPlatform.isLinux [
    # Archive
    pkgs.p7zip     # 7zip for linux
    pkgs.unrar     # Still need it
  ];

  home = {
    sessionVariables = {
      PROJ_ROOT = config.xdg.userDirs.documents;   # Read by `project` to jump between projects
    } // lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
      CLICOLOR = 1;
    };

    shellAliases = {
      l = "${lib.getExe pkgs.eza} -alh";
    };
  };
}
