{ lib, pkgs, ... }:
{
  # emulationstation  # Does not as it needs additional configuration: perhaps FIXME: https://github.com/juliosueiras-nix/nix-emulationstation/blob/master/modules/emulationstation/default.nix?
  home.packages = [
    # More cores at: https://github.com/NixOS/nixpkgs/tree/master/pkgs/applications/emulators/libretro/cores
    # Emulation
    # Bios in XDG_CONFIG_DIR/retroarch/system
    (pkgs.retroarch.withCores (cores: with cores; [
      genesis-plus-gx # Megadrive
      snes9x          # Snes
      swanstation     # PSX
      mgba            # GBA
      desmume         # NDS
      dosbox-pure     # DOS
      prboom          # Doom
      fbneo           # Arcade
      flycast         # Dreamcast
      gambatte
      #N64 - Mupen64Plus-Next with Parallel-RDP - Mupen64Plus-Next GLES3 with GlideN64
    ]))
    pkgs.mame-tools  # Convert to CHD: parallel chdman createcd -i {} -o {.}.chd ::: *.iso
    pkgs.maxcso      # To convert to CSO
  ];
}