{ pkgs, config, lib, ... }:
{
  stylix.targets.yazi.enable = true;
  programs.yazi = {
    enable = true;
    shellWrapperName = "y";

    settings = {
      mgr = {
        sort_by = "natural";
        sort_dir_first = true;
      };

      preview = {
        cache_dir = "${config.xdg.cacheHome}/yazi";
      };
    };

    keymap.mgr.prepend_keymap = [
      { on   = "!"; run  = ''shell "$SHELL" --block --confirm''; desc = "Open shell here"; }
    ];

    theme.filetype.rules = lib.mkBefore [
      { mime = "*"; is = "orphan"; bg = "red"; }        # Highlight orphaned files
    ];
  };

  systemd.user.tmpfiles.rules = lib.optionals pkgs.stdenv.hostPlatform.isLinux [
    "d ${config.xdg.cacheHome}/yazi 700 ${config.home.username} users 10d -"
  ];

  custom.programs.niri.bindings = lib.optionalAttrs pkgs.stdenv.hostPlatform.isLinux {
    "Mod+E" = ''spawn-sh "${config.custom.programs.file-explorer.browser}"'';
  };
}
