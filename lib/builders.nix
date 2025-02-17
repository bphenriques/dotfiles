{ lib }:
{
  writeDmenuScript = pkgs: {
    name,
    dmenu ? ''${lib.getExe pkgs.fuzzel} --dmenu --width 15'',
    entries
  }: pkgs.writeShellApplication {
      inherit name;
      text = ''
        #shellcheck shell=bash

        chosen="$(echo -e "${lib.concatMapStringsSep "\\n" (entry: entry.label) entries}" | ${dmenu} -l ${toString (builtins.length entries)})"
        case ''${chosen} in
          ${lib.concatMapStringsSep "\n" (entry: ''"${entry.label}") ${entry.exec} ;;'') entries}
        esac
      '';
      meta.platforms = lib.platforms.linux;
    };
}
