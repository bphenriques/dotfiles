{ lib, pkgs }:
rec {
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

  mkZshPlugin = { drv, zshPlugin }:
    pkgs.symlinkJoin {
      inherit (drv) name;
      paths = [ drv ];
      postBuild = ''
        install -Dm644 ${zshPlugin} $out/share/zsh/plugins/${drv.name}/${drv.name}.plugin.zsh
      '';
      meta.platforms = lib.platforms.all;
    };

  writeNushellScript = name: src:
    pkgs.runCommand "${name}-checked" { } ''
      ${lib.getExe pkgs.nushell} --no-config-file --commands 'if not (nu-check "${src}") { exit 1 }'
      cp ${src} $out
    '';
}