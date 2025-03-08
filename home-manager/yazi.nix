{ pkgs, config, lib, ... }:
let
  downloadDirName = "downloads";
in
{
  # TODO: https://github.com/bigbabyboost/dotfiles/blob/hyprnix/home/terminal/programs/yazi/default.nix
  # Default: https://github.com/sxyazi/yazi/blob/shipped/yazi-config/preset/yazi-default.toml
  stylix.targets.yazi.enable = true;
  programs.yazi = {
    enable = true;
    enableFishIntegration = true;
    shellWrapperName = "y";

    settings = {
      manager = {
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

  xdg.configFile = {
    # TODO: Migrate to hm types
    # Smart sorting depending on the directory
    "yazi/plugins/folder-rules.yazi/main.lua".text = ''
      local function setup()
        ps.sub("cd", function()
          local cwd = cx.active.current.cwd
          if cwd:ends_with("${downloadDirName}") then
            ya.manager_emit("sort", { "modified", reverse = true, dir_first = false })
          else
            ya.manager_emit(
              "sort",
              {
                sort_by = "${config.programs.yazi.settings.manager.sort_by}",
                reverse = false,
                dir_first = ${toString config.programs.yazi.settings.manager.sort_dir_first}
              }
            )
          end
        end)
      end

      return { setup = setup }
    '';

    "yazi/init.lua".text = ''
      require("folder-rules"):setup()
    '';
  };

  systemd.user.tmpfiles.rules = lib.optionals pkgs.stdenv.isLinux [
    "d ${config.xdg.cacheHome}/yazi 600 ${config.home.username} users 10d -"
  ];
}
