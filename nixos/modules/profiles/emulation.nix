{ pkgs, config, lib, ... }:

with lib;
let
  cfg = config.custom.profiles.emulation;
in
{
  options.custom.profiles.emulation = with types; {
    enable = mkEnableOption "emulation profile";
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      # Emulation
      # Bios in XDG_CONFIG_DIR/retroarch/system
      # emulationstation  # Does not as it needs additional configuration: perhaps https://github.com/juliosueiras-nix/nix-emulationstation/blob/master/modules/emulationstation/default.nix?
      (retroarch.override { # more: https://github.com/NixOS/nixpkgs/blob/master/pkgs/applications/emulators/retroarch/cores.nix
        cores = with libretro; [
          genesis-plus-gx # Megadrive
          snes9x          # Snes
          swanstation     # PSX
          mgba            # GBA
          desmume         # NDS
          dosbox-pure     # DOS
          prboom          # Doom
          fbneo           # Most Arcade games
          flycast         # Dremcast
          gambatte
          #N64 - Mupen64Plus-Next with Parallel-RDP - Mupen64Plus-Next GLES3 with GlideN64
          # bsnes
          # mesen
          # PPSSPP
        ];
      })
      mame-tools  # Convert to CHD: parallel chdman createcd -i {} -o {.}.chd ::: *.iso
      maxcso      # To convert to CSO
    ];
  };
}
