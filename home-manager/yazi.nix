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

  home.packages = [
    (pkgs.makeDesktopItem {
      name = "Files";
      desktopName = "Files";
      #icon = "Files";
      exec = "${lib.getExe pkgs.ghostty} -e ${lib.getExe config.programs.yazi.package}";
      terminal = false;
      #DBusActivatable=true
      #StartupNotify=true
      #Actions=new-window TODO: Likely add shortcuts?
      # TODO: Can I actually reuse the Yazi desktop entry?
      mimeTypes = [ "inode/directory" "application/x-7z-compressed" "application/x-7z-compressed-tar" "application/x-bzip" "application/x-bzip-compressed-tar" "application/x-compress" "application/x-compressed-tar" "application/x-cpio" "application/x-gzip" "application/x-lha" "application/x-lzip" "application/x-lzip-compressed-tar" "application/x-lzma" "application/x-lzma-compressed-tar" "application/x-tar" "application/x-tarz" "application/x-xar" "application/x-xz" "application/x-xz-compressed-tar" "application/zip" "application/gzip" "application/bzip2" "application/x-bzip2-compressed-tar" "application/vnd.rar" "application/zstd" "application/x-zstd-compressed-tar" ];
      categories = [ "Utility" "Core" "FileTransfer" ];
    })
  ];
  custom.xdgDefaultApps.fileBrowser = lib.mkBefore [ "yazi.desktop" ];
}
