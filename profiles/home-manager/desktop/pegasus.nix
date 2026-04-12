{ lib, pkgs, config, osConfig, ... }:
let
  emulationPaths = osConfig.custom.homelab.paths.media.gaming.emulation;
  retroarchPackage = config.programs.retroarch.finalPackage;
  retroarch = lib.getExe' retroarchPackage "retroarch";
  coresDir = "${retroarchPackage}/lib/retroarch/cores";

  gameOS-theme = pkgs.fetchFromGitHub {
    owner = "PlayingKarrde";
    repo = "gameOS";
    rev = "7a5a5223ff7371d0747a7c5d3a3b8f2f5e36b4f2";
    hash = "sha256-EBpIe0aw1FO7DzB6F3oAWD5FRLF2iZGtOHllMxuamdc=";
  };

  # Each entry maps a ROM directory to a Pegasus collection and RetroArch core.
  # `shortname` defaults to `dir` when omitted.
  systems = [
    { dir = "megadrive"; name = "Megadrive";        core = "genesis_plus_gx_libretro.so";   extensions = "md"; }
    { dir = "nes";       name = "NES";              core = "fceumm_libretro.so";            extensions = "nes"; }
    { dir = "snes";      name = "Super Nintendo";   core = "snes9x_libretro.so";            extensions = "smc, sfc"; }
    { dir = "psx";       name = "PlayStation";      core = "swanstation_libretro.so";       extensions = "chd, m3u"; }
    { dir = "gb";        name = "Game Boy";         core = "gambatte_libretro.so";          extensions = "gb"; }
    { dir = "gbc";       name = "Game Boy Color";   core = "gambatte_libretro.so";          extensions = "gbc"; }
    { dir = "gba";       name = "Game Boy Advance"; core = "mgba_libretro.so";              extensions = "gba"; }
    { dir = "nds";       name = "Nintendo DS";      core = "desmume_libretro.so";           extensions = "nds"; }
    { dir = "dos";       name = "DOS";              core = "dosbox_pure_libretro.so";       extensions = "zip"; }
    { dir = "doom";      name = "Doom";             core = "prboom_libretro.so";            extensions = "wad"; }
    { dir = "fbneo";     name = "Arcade";           core = "fbneo_libretro.so";             extensions = "zip"; shortname = "arcade"; }
    { dir = "dreamcast"; name = "Dreamcast";        core = "flycast_libretro.so";           extensions = "chd"; }
  ];

  # Skyscraper folder name → Pegasus asset key

  # Maps Skyscraper folder to Pegasus assert. Required for the script that writes the pegasus metadata
  artworkMapping = [
    { folder = "covers";      asset = "box_front"; }
    { folder = "screenshots"; asset = "screenshot"; }
    { folder = "wheels";      asset = "logo"; }
    { folder = "marquees";    asset = "marquee"; }
    { folder = "textures";    asset = "background"; }
  ];
  mkSystemScript = sys: let
    romsDir = "${emulationPaths.roms}/${sys.dir}";
    exts = lib.splitString ", " sys.extensions;
    findArgs = lib.concatMapStringsSep " -o " (e: "-name '*.${e}'") exts;
    assetChecks = lib.concatMapStringsSep "\n" (a: ''
      asset="${romsDir}/media/${a.folder}/$basename.png"
      if [[ -f "$asset" ]]; then printf 'assets.${a.asset}: %s\n' "$asset" >> "$out"; fi
    '') artworkMapping;
  in ''
    # ${sys.name}
    out="$METAFILES_DIR/${sys.dir}.metadata.pegasus.txt"
    if [[ -d "${romsDir}" ]]; then
      {
        printf 'collection: ${sys.name}\n'
        printf 'shortname: ${sys.shortname or sys.dir}\n'
        printf 'launch: ${retroarch} -L ${coresDir}/${sys.core} "{file.path}"\n'
        printf '\n'
      } > "$out"

      find "${romsDir}" -maxdepth 1 -type f \( ${findArgs} \) | sort | while read -r rom; do
        basename="''${rom##*/}"
        basename="''${basename%.*}"
        printf '\ngame: %s\nfile: %s\n' "$basename" "$rom" >> "$out"
        ${assetChecks}
      done
    fi
  '';

  generatePegasusMetadata = pkgs.writeShellApplication {
    name = "generate-pegasus-metadata";
    runtimeInputs = [ pkgs.findutils ];
    meta.platforms = lib.platforms.linux;
    text = ''
      METAFILES_DIR="${config.xdg.configHome}/pegasus-frontend/metafiles"
      mkdir -p "$METAFILES_DIR"
      ${lib.concatMapStringsSep "\n" mkSystemScript systems}
      printf 'Pegasus metadata generated in %s\n' "$METAFILES_DIR"
    '';
  };
in
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [
    pkgs.pegasus-frontend
    generatePegasusMetadata
  ];

  xdg.configFile = {
    "pegasus-frontend/themes/gameOS".source = gameOS-theme;
    "pegasus-frontend/settings.txt".text = ''
      general.theme: themes/gameOS
    '';
  };
}
