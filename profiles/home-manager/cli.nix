{ pkgs, lib, osConfig, ... }:
{
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
    settings = {
      "*" = {
        SetEnv.TERM = "xterm-256color";   # Sane default across different terminals. Don't need more.
        AddKeysToAgent = "4h";            # Cache the keys temporarily but expire after 4 hours.
      };
      "bruno-home-nas" = {
        User = "Bruno-Admin";
        Port = 6188;
      };
      "pi-zero".User = "pi";
      "rg353m".User = "ark";
      "pixel".User = "bruno";
      "share-vm" = {
        HostName = osConfig.custom.fleet.microvmHosts."share-vm";
        User = "bphenriques";
        ProxyJump = "compute";
      };
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
    # Archive
    pkgs.p7zip     # 7zip for linux
    pkgs.unrar     # Still need it
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
