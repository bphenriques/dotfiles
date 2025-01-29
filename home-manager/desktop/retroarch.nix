{ lib, pkgs, ... }:
lib.mkIf pkgs.stdenv.isLinux {
  home.packages = [
    # More cores at: https://github.com/NixOS/nixpkgs/tree/master/pkgs/applications/emulators/libretro/cores
    # Emulation
    # Bios in XDG_CONFIG_DIR/retroarch/system
    (pkgs.retroarch.withCores (cores: with cores; [
      genesis-plus-gx # Megadrive
      snes9x          # Snes
      swanstation     # PSX
      gambatte        # Gameboy (Color)
      mgba            # GBA
      desmume         # NDS
      dosbox-pure     # DOS
      prboom          # Doom
      fbneo           # Arcade
      flycast         # Dreamcast
    ]))
    pkgs.mame-tools  # Convert to CHD: parallel chdman createcd -i {} -o {.}.chd ::: *.iso
    pkgs.maxcso      # To convert to CSO
  ];
}