{ pkgs, config, lib, ... }:
let
  files-browser = "${lib.getExe pkgs.ghostty} +new-window --title=yazi-tui -e ${lib.getExe config.programs.yazi.package}";
in
{
  stylix.targets.yazi.enable = true;
  programs.yazi = {
    enable = true;
    enableFishIntegration = true;
    shellWrapperName = "y";

    settings = {
      mgr = {
        sort_by = "natural";
        sort_dir_first = true;

        prepend_keymap = [
          { on   = "!"; run  = ''shell "$SHELL" --block --confirm''; desc = "Open shell here"; }
        ];
      };

      preview = {
        cache_dir = "${config.xdg.cacheHome}/yazi";
      };
    };

    theme.filetype.rules = lib.mkBefore [
      { mime = "*"; is = "orphan"; bg = "red"; }        # Highlight orphaned files
    ];
  };

  systemd.user.tmpfiles.rules = lib.optionals pkgs.stdenv.isLinux [
    "d ${config.xdg.cacheHome}/yazi 600 ${config.home.username} users 10d -"
  ];

  custom.programs.niri.bindings = lib.optionalAttrs pkgs.stdenv.isLinux {
    "Mod+E" = ''spawn-sh "${files-browser}"'';
  };
}
