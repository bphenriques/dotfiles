{ pkgs, lib, osConfig, ... }:
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
  programs.nushell.enable = true;         # Adhoc shell for data processing
  programs.bash.enable = true;            # Login/ssh shell; without it home.sessionVariables reach fish only

  programs.ssh = {
    enable = true;
    enableDefaultConfig = false;
    includes = [ "$HOME/.ssh/config.local" ];
    settings = {
      "*" = {
        SetEnv.TERM = "xterm-256color";   # Sane default across different terminals. Don't need more.
        AddKeysToAgent = "4h";            # Cache the keys temporarily but expire after 4 hours.
      };
      "pi-zero".User = "pi";
      "rg353m".User = "ark";
      "pixel".User = "bruno";
      # accept-new: persistent key, so a one-time bootstrap TOFU; a *changed* key is still refused.
      "compute".StrictHostKeyChecking = "accept-new";
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
    enable = pkgs.stdenv.hostPlatform.isLinux;
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

  programs.vivid.enable = true;           # `LS_COLORS` generator
  programs.vivid.activeTheme = "one-dark";

  programs.bat.enable = true;             # Better file previewer
  stylix.targets.bat.enable = true;

  programs.fd.enable = true;              # Better `find`.
  my.programs.fzf-fd.enable = true;   # Fuzzy fd
  programs.jq.enable = true;              # JSON query.
  my.programs.project.enable = true;  # Easier way to navigate jump through different projects
  my.programs.fzf-rg.enable = true;   # Fuzzy ripgrep

  home.packages = lib.optionals pkgs.stdenv.hostPlatform.isLinux [
    # Archive
    pkgs.p7zip     # 7zip for linux
    pkgs.unrar     # Still need it
  ];

  home = {
    sessionVariables = lib.optionalAttrs pkgs.stdenv.hostPlatform.isDarwin {
      CLICOLOR = 1;
    };

    shellAliases = {
      l = "${lib.getExe pkgs.eza} -alh";
    };
  };
}
