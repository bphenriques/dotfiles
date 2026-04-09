{ lib, pkgs }:
{
  # Option type for app executables: accepts a package (extracts the main binary) or a raw string path.
  mkAppOpt = default: lib.mkOption {
    inherit default;
    description = "";
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  writeFuzzelDmenuApplication = {
    name,
    runtimeInputs ? [ ],
    package ? pkgs.fuzzel,
    extraArgs ? "",
    entries
  }: pkgs.writeShellApplication {
      inherit name runtimeInputs;
      text = let
        options = lib.concatMapStringsSep "\n" (entry: entry.label) entries;
        exec = "${lib.getExe package} --dmenu ${extraArgs}";
      in ''
        #shellcheck shell=bash

        chosen="$(printf '%s' "${options}" | ${exec})"
        case ''${chosen} in
          ${lib.concatMapStringsSep "\n" (entry: ''"${entry.label}") ${entry.exec} ;;'') entries}
        esac
      '';
      meta.platforms = lib.platforms.linux;
    };

  # Good enough: I can point directly to the generated file.
  mkNerdFontIcon = { textColor ? "black", size ? "128x128", fontSize ? 160 }: name: symbol:
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

  mkFishShellPlugin = { drv, fishPluginSrc }:
    pkgs.symlinkJoin {
      inherit (drv) name;
      paths = [ drv ];
      postBuild = ''
        if [ -d "${fishPluginSrc}/functions" ]; then
          mkdir -p $out/share/fish/vendor_functions.d
          cp ${fishPluginSrc}/functions/*.fish $out/share/fish/vendor_functions.d/
        fi
        if [ -d "${fishPluginSrc}/conf.d" ]; then
          mkdir -p $out/share/fish/vendor_conf.d
          cp ${fishPluginSrc}/conf.d/*.fish $out/share/fish/vendor_conf.d/
        fi
      '';
      meta.platforms = lib.platforms.all;
    };

  writeNushellScript = name: src:
    pkgs.runCommand "${name}-checked" { } ''
      ${lib.getExe pkgs.nushell} --no-config-file --commands 'if not (nu-check "${src}") { exit 1 }'
      cp ${src} $out
    '';
}