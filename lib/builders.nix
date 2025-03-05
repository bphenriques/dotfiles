{ lib }:
{
  writeDmenuApplication = pkgs: {
    name,
    dmenu ? ''${lib.getExe pkgs.fuzzel} --dmenu'',
    placeholder ? "",
    entries
  }: pkgs.writeShellApplication {
      inherit name;
      text = let
        options = lib.concatMapStringsSep "\\n" (entry: entry.label) entries;
        exec = "${dmenu} --placeholder ${placeholder} -l ${toString (builtins.length entries)}";
      in ''
        #shellcheck shell=bash

        chosen="$(echo -e "${options}" | ${exec})"
        case ''${chosen} in
          ${lib.concatMapStringsSep "\n" (entry: ''"${entry.label}") ${entry.exec} ;;'') entries}
        esac
      '';
      meta.platforms = lib.platforms.linux;
    };

  # Good enough: this could be svg to be resisable. But then it would have to be installed following a dir convention.
  # As it is, I can point directly to the generated file.
  mkNerdFontIcon = pkgs: { textColor ? "black", size ? "128x128", fontSize ? 160 }: name: symbol:
    let
      fontFile = "${pkgs.nerd-fonts.hack}/share/fonts/truetype/NerdFonts/Hack/HackNerdFontMono-Regular.ttf";
      derivation = pkgs.runCommand "${name}-custom-icon.png" { } ''
        export XDG_CACHE_HOME="$(mktemp -d)"
        ${lib.getExe pkgs.imagemagick} -size ${size} xc:none \
          -font "${fontFile}" -pointsize ${toString fontSize} -fill "${textColor}" -gravity center \
          -draw "text 0,0 '${symbol}'" \
          png32:$out;
      '';
    in toString derivation; # only care about the path to the png file
}
