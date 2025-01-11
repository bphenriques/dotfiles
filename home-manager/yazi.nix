{ pkgs, config, lib, ... }:
let
  downloadDirName = "downloads";
in
{
  programs.yazi = {
    enable = true;
    enableFishIntegration = true;
    shellWrapperName = "y";

    settings = {
      manager = {
        sort_by = "natural";
        sort_dir_first = true;

        prepend_keymap = [
          {
            on   = "l";
            run  = "plugin --sync smart-enter";
            desc = "Enter the child directory, or open the file";
          }
          {
            on   = "!";
            run  = ''shell "$SHELL" --block --confirm'';
            desc = "Open shell here";
          }
        ];
      };
    };
  };

  # To explore:
  # - https://github.com/yazi-rs/plugins/tree/main/jump-to-char.yazi
  # - https://github.com/yazi-rs/plugins/tree/main/no-status.yazi
  # - https://github.com/yazi-rs/plugins/tree/main/smart-filter.yazi
  xdg.configFile = {
    # Smart sorting depending on the directory
    "yazi/plugins/folder-rules.yazi/init.lua".text = ''
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

  custom.xdgDefaultApps.fileBrowser = lib.mkBefore [ "yazi.desktop" ];
}
