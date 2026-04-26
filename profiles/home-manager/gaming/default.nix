{ lib, pkgs, ... }:
{
  imports = [
    ./mangohud.nix          # Game HUD
    ./retroarch.nix         # Emulation
    ./pegasus.nix           # Game launcher frontend
    ./steam-rom-manager.nix # Steam ROM shortcuts
    ./ppsspp.nix            # PSP emulator
    ./pcsx2.nix             # PS2 emulator
    ./dolphin.nix           # Wii/GameCube emulator
    ./heroic.nix            # Unified game client
    ./umu-launcher.nix      # Ad-hoc game launcher
  ];

  # Workspace 3 is the gaming workspace: fullscreen, no transparency.
  custom.programs.niri.windowRules = {
    byApp = [
      ''
        window-rule {
          match app-id="com.libretro.RetroArch"
          match app-id="ppsspp"
          match app-id="PCSX2"
          match app-id="dolphin-emu"
          match app-id=r#"^steam_app"#
          open-on-workspace "3"
          open-fullscreen true
          open-focused true
        }
      ''
      ''
        window-rule {
          match app-id="Steam"
          open-on-workspace "3"
          open-maximized true
          scroll-factor 0.5
        }
      ''
    ];

    byType.popups = [
      ''app-id="Steam" title=r#"^Steam .+"#''
    ];

    overrides = [
      ''
        window-rule {
          match app-id="com.libretro.RetroArch"
          match app-id="ppsspp"
          match app-id="PCSX2"
          match app-id="dolphin-emu"
          match app-id=r#"^steam_app"#
          match app-id="Steam"
          opacity 1.0
        }
      ''
    ];
  };
}
