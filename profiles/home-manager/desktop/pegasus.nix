{ lib, pkgs, config, osConfig, self, ... }:
let
  gameOS-theme = pkgs.fetchFromGitHub {
    owner = "PlayingKarrde";
    repo = "gameOS";
    rev = "7a5a5223ff7371d0747a7c5d3a3b8f2f5e36b4f2";
    hash = "sha256-EBpIe0aw1FO7DzB6F3oAWD5FRLF2iZGtOHllMxuamdc=";
  };

  # Maps a ROM directory to a Pegasus collection and a launch command (retroarch using the `core` key, or `launch` for other executables).
  systems = [
    { dir = "megadrive"; name = "Megadrive";        core = "genesis_plus_gx"; extensions = [ "md" ]; }
    { dir = "nes";       name = "NES";              core = "fceumm";          extensions = [ "nes" ]; }
    { dir = "snes";      name = "Super Nintendo";   core = "snes9x";          extensions = [ "smc" "sfc" ]; }
    { dir = "psx";       name = "PlayStation";      core = "swanstation";     extensions = [ "chd" "m3u" ]; }
    { dir = "gb";        name = "Game Boy";         core = "gambatte";        extensions = [ "gb" ]; }
    { dir = "gbc";       name = "Game Boy Color";   core = "gambatte";        extensions = [ "gbc" ]; }
    { dir = "gba";       name = "Game Boy Advance"; core = "mgba";            extensions = [ "gba" ]; }
    { dir = "nds";       name = "Nintendo DS";      core = "desmume";         extensions = [ "nds" ]; }
    { dir = "dos";       name = "DOS";              core = "dosbox_pure";     extensions = [ "zip" ]; }
    { dir = "doom";      name = "Doom";             core = "prboom";          extensions = [ "wad" ]; }
    { dir = "fbneo";     name = "Arcade";           core = "fbneo";           extensions = [ "zip" ]; shortname = "arcade"; }
    { dir = "dreamcast"; name = "Dreamcast";        core = "flycast";         extensions = [ "chd" ]; }
    #{ dir = "dreamcast"; name = "Dreamcast";        launch = "${lib.getExe pkgs.flycast} \"{file.path}\"";  extensions = [ "chd" ]; }
    { dir = "psp";       name = "PSP";              launch = "${lib.getExe pkgs.ppsspp} --fullscreen --pause-menu-exit \"{file.path}\"";  extensions = [ "iso" "cso" ]; }
    { dir = "ps2";       name = "PlayStation 2";    launch = "${lib.getExe pkgs.pcsx2} -fullscreen -fastboot -batch -- \"{file.path}\"";  extensions = [ "iso" "chd" ]; }
    { dir = "wii";       name = "Wii";              launch = "${lib.getExe pkgs.dolphin-emu} -b -e \"{file.path}\"";                      extensions = [ "iso" "wbfs" "rvz" ]; }
  ];

  retroarchPackage = config.programs.retroarch.finalPackage;
  coresDir = "${retroarchPackage}/lib/retroarch/cores";
  mkLaunchCmd = sys:
    if sys ? core
    then "${lib.getExe' retroarchPackage "retroarch"} -L ${coresDir}/${sys.core}_libretro.so \"{file.path}\""
    else sys.launch;
  configFile = pkgs.writeText "pegasus-metadata-config.json" (builtins.toJSON {
    romsDir = osConfig.custom.homelab.paths.media.gaming.emulation.roms;

    # Maps Skyscraper folder to Pegasus asset
    artworkMapping = [
      { folder = "covers";      asset = "box_front"; }
      { folder = "screenshots"; asset = "screenshot"; }
      { folder = "wheels";      asset = "logo"; }
      { folder = "marquees";    asset = "marquee"; }
      { folder = "textures";    asset = "background"; }
   ];
    systems = map (sys: {
      inherit (sys) dir name extensions;
      shortname = sys.shortname or sys.dir;
      launch = mkLaunchCmd sys;
    }) systems;
  });

  metafilesDir = "${config.xdg.configHome}/pegasus-frontend/metafiles";
in
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [ pkgs.pegasus-frontend ];

  systemd.user = {
    services.generate-pegasus-metadata = {
      Unit.Description = "Generate Pegasus frontend metadata files";
      Service = {
        Type = "oneshot";
        ExecStart = lib.escapeShellArgs [ (lib.getExe self.packages.generate-pegasus-metadata) configFile metafilesDir ];
      };
    };

    timers.generate-pegasus-metadata = {
      Unit.Description = "Generate Pegasus metadata weekly";
      Install.WantedBy = [ "timers.target" ];
      Timer = {
        OnCalendar = "weekly";
        RandomizedDelaySec = "6h";
        Persistent = true;
      };
    };
  };

  xdg.configFile = {
    "pegasus-frontend/themes/gameOS".source = gameOS-theme;
    "pegasus-frontend/settings.txt".text = ''
      general.theme: themes/gameOS
    '';
  };
}
