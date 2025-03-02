{ lib }:
{
  writeDmenuScript = pkgs: {
    name,
    dmenu ? ''${lib.getExe pkgs.fuzzel} --dmenu'',
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

  # Good enough for ad-hoc icons that need to be in PNG
  mkNerdFontIcon = pkgs: name: symbol:
    let
      fontFile = "${pkgs.nerd-fonts.hack}/share/fonts/truetype/NerdFonts/Hack/HackNerdFontMono-Regular.ttf";
      derivation = pkgs.runCommand "${name}-custom-icon.png" { } ''
        export XDG_CACHE_HOME="$(mktemp -d)"
        ${lib.getExe pkgs.imagemagick} -size 128x128 xc:none -fill white -stroke black \
          -draw "roundrectangle 0,0 128,128 30,3g0" \
          -font "${fontFile}" -pointsize 140 -fill black -gravity center \
          -draw "text 0,0 '${symbol}'" \
          png32:$out;
      '';
    in toString derivation; # only care about the path to the png file
}
