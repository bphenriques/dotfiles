{ lib, pkgs, osConfig, ... }:
let
  gameOS-theme = pkgs.fetchFromGitHub {
    owner = "PlayingKarrde";
    repo = "gameOS";
    rev = "7a5a5223ff7371d0747a7c5d3a3b8f2f5e36b4f2";
    hash = "sha256-EBpIe0aw1FO7DzB6F3oAWD5FRLF2iZGtOHllMxuamdc=";
  };

  romsDir = "${osConfig.fleet.shares.media.root}/gaming/emulation/roms";

  # Games come from RomM's export; `name` must match RomM's so collections merge, and this only adds the launch command.
  systems = [
    { dir = "megadrive"; name = "Sega Genesis";                        core = "genesis_plus_gx"; }
    { dir = "nes";       name = "Nintendo Entertainment System";       core = "fceumm"; }
    { dir = "snes";      name = "Super Nintendo Entertainment System"; core = "snes9x"; }
    { dir = "psx";       name = "PlayStation";                         core = "swanstation"; }
    { dir = "gb";        name = "Game Boy";                            core = "gambatte"; }
    { dir = "gbc";       name = "Game Boy Color";                      core = "gambatte"; }
    { dir = "gba";       name = "Game Boy Advance";                    core = "mgba"; }
    { dir = "nds";       name = "Nintendo DS";                         core = "desmume"; }
    { dir = "dos";       name = "DOS";                                 core = "dosbox_pure"; }
    { dir = "doom";      name = "Doom";                                core = "prboom"; }
    { dir = "fbneo";     name = "Arcade";                              core = "fbneo"; }
    { dir = "dreamcast"; name = "Sega Dreamcast";                      core = "flycast"; }
    { dir = "psp";       name = "PlayStation Portable";                launch = "${lib.getExe pkgs.ppsspp} --fullscreen --pause-menu-exit \"{file.path}\""; }
    { dir = "ps2";       name = "PlayStation 2";                       launch = "${lib.getExe pkgs.pcsx2} -fullscreen -fastboot -batch -- \"{file.path}\""; }
    { dir = "wii";       name = "Nintendo Wii";                        launch = "${lib.getExe pkgs.dolphin-emu} -b -e \"{file.path}\""; }
  ];

  # Paths are solved in runtime. Assumes bin/retroarch and lib/retroarch/cores live under the package root.
  retroarchWithCore = pkgs.writeShellApplication {
    name = "retroarch-with-core";
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      retroarch_bin="$(readlink -f "$(command -v retroarch)")"
      core_path="''${retroarch_bin%/bin/retroarch}/lib/retroarch/cores/''${1}_libretro.so"
      shift
      exec "$retroarch_bin" -L "$core_path" "$@"
    '';
  };

  mkLaunchCmd = sys:
    if sys ? core
    then "${lib.getExe retroarchWithCore} ${sys.core} \"{file.path}\""
    else sys.launch;
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  home.packages = [ pkgs.pegasus-frontend ];

  xdg.configFile = {
    "pegasus-frontend/themes/gameOS".source = gameOS-theme;
    "pegasus-frontend/game_dirs.txt".text = lib.concatMapStrings (sys: "${romsDir}/${sys.dir}\n") systems;
    "pegasus-frontend/metafiles/launch.metadata.pegasus.txt".text =
      lib.concatMapStrings (sys: "collection: ${sys.name}\nlaunch: ${mkLaunchCmd sys}\n\n") systems;
    # Games come only from RomM's export, so the importers for other launchers' libraries stay off.
    "pegasus-frontend/settings.txt".text = ''
      general.theme: themes/gameOS
    '' + lib.concatMapStrings (p: "providers.${p}.enabled: false\n")
      [ "steam" "gog" "es2" "launchbox" "logiqx" "lutris" "playnite" "skraper" ];
  };
}
