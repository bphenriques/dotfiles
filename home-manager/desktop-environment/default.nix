{ config, lib, pkgs, self, ... }:
let
  mkIcon = name: symbol: self.lib.builders.mkNerdFontIcon pkgs { textColor = config.lib.stylix.colors.withHashtag.base07; } name symbol;
in
{
  imports = [
    ./fuzzel.nix            # Application launcher
    ./wlr-which-key.nix     # Which.key as regular overlays
    ./mako.nix              # Notifications
    ./swayidle.nix          # Idle behaviour
    ./niri.nix              # Window Manager
  ];

  custom.programs.screenshot.enable = true;
  custom.programs.screen-recorder.enable = true;
  custom.programs.volume-osd.enable = true;
  custom.programs.file-explorer.browser = "${lib.getExe' pkgs.foot "footclient"} --title=yazi-tui ${lib.getExe pkgs.yazi}";
  custom.programs.powerprofilesctl.enable = true;
  custom.programs.volume-osd.package = self.pkgs.volume-osd.override {
    headphonesIcon = mkIcon "volume-osd-headphones" "";
    headphonesMuteIcon = mkIcon "volume-osd-headphones" "󰋐";
    headsetIcon = mkIcon "volume-osd-headset" "󰋎";
    headsetMuteIcon = mkIcon "volume-osd-headset-mute" "󰋐";
    internalSpeakersIcon = mkIcon "volume-osd-internal" "󰽟";
    internalSpeakersMuteIcon = mkIcon "volume-osd-internal-mute" "󰽠";
    externalSpeakersIcon = mkIcon "volume-osd-external" "󰓃";
    externalSpeakersMuteIcon = mkIcon "volume-osd-external-mute" "󰓄";
  };

  home.packages = [
    pkgs.wdisplays

    # Management
    self.pkgs.brightness-osd
    self.pkgs.niri-keyboard-layout
  ];
}