_:
{
  imports = [
    ./stylix.nix          # Color scheme, fonts, icons, cursor
    ./ghostty.nix         # Terminal
    ./obsidian.nix        # note taking
  ];

  # Enable easier font management
  fonts.fontconfig.enable = true;
  stylix.targets.fontconfig.enable = true;
}
