{ lib, pkgs, config, osConfig, self, ... }:
let
  cfg = config.custom.programs.volume-osd;

  volume-osd = lib.getExe cfg.package;
  mkIcon = self.lib.builders.mkNerdFontIcon pkgs { textColor = config.lib.stylix.colors.withHashtag.base07; };
in
{
  options.custom.programs.volume-osd = {
    enable = lib.mkEnableOption "custom-volume-osd";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.pkgs.volume-osd.override {
        headphonesIcon = mkIcon "volume-osd-headphones" "";
        headphonesMuteIcon = mkIcon "volume-osd-headphones" "󰋐";
        headsetIcon = mkIcon "volume-osd-headset" "󰋎";
        headsetMuteIcon = mkIcon "volume-osd-headset-mute" "󰋐";
        internalSpeakersIcon = mkIcon "volume-osd-internal" "󰽟";
        internalSpeakersMuteIcon = mkIcon "volume-osd-internal-mute" "󰽠";
        externalSpeakersIcon = mkIcon "volume-osd-external" "󰓃";
        externalSpeakersMuteIcon = mkIcon "volume-osd-external-mute" "󰓄";
        microphoneIcon = mkIcon "volume-osd-micro" "󰍬";
        microphoneMuteIcon = mkIcon "volume-osd-micro-mute" "󰍭";
        errorIcon = mkIcon "volume-osd-error-icon" "";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [
      (lib.hm.assertions.assertPlatform "custom.programs.custom-volume-osd" pkgs lib.platforms.linux)
      { assertion = osConfig.services.pipewire.enable; message = "Requires pipewire enabled in the system."; }
      { assertion = osConfig.services.pipewire.pulse.enable; message = "Requires pipewire-pulse enabled in the system."; }
      { assertion = osConfig.services.pipewire.wireplumber.enable; message = "Requires pipewire wireplumber enabled in the system."; }
    ];

    custom.programs.wlr-which-key.menus = {
      sound-output = [
        { key = ["Up" "k"];       desc = "Increase volume"; cmd = "${volume-osd} sink-increase";    keep_open = true; }
        { key = ["Down" "j"];     desc = "Reduce volume";   cmd = "${volume-osd} sink-decrease";    keep_open = true; }
        { key = ["Left" "h"];     desc = "Previous device"; cmd = "${volume-osd} sink-move-prev";   keep_open = true; }
        { key = "m";              desc = "Mute";            cmd = "${volume-osd} sink-toggle-mute"; keep_open = true; }
        { key = "space";          desc = "Select device";   cmd = "${volume-osd} sink-move-fuzzel"; }
        { key = ["Right" "l"];    desc = "Next device";     cmd = "${volume-osd} sink-move-next";   keep_open = true; }
        { key = "c";              desc = "Configure";       cmd = "${lib.getExe pkgs.pavucontrol} --tab=3"; }
      ];

      sound-input = [
        { key = ["Up" "k"];       desc = "Increase volume"; cmd = "${volume-osd} source-increase";    keep_open = true; }
        { key = ["Down" "j"];     desc = "Reduce volume";   cmd = "${volume-osd} source-decrease";    keep_open = true; }
        { key = "m";              desc = "Mute";            cmd = "${volume-osd} source-toggle-mute"; keep_open = true; }
        { key = ["Left" "h"];     desc = "Previous device"; cmd = "${volume-osd} source-move-prev";   keep_open = true; }
        { key = ["Right" "l"];    desc = "Next device";     cmd = "${volume-osd} source-move-next";   keep_open = true; }
        { key = "space";          desc = "Select device";   cmd = "${volume-osd} source-move-fuzzel"; }
        { key = "c";              desc = "Configure";       cmd = "${lib.getExe pkgs.pavucontrol} --tab=4"; }
      ];
    };

    home.packages = [
      cfg.package

      # Using pulseaudio tools without actually using pulseaudio
      pkgs.pavucontrol  # UI
      pkgs.pipewire     # Prefered command line interface
      pkgs.pulseaudio   # Other command line interface
    ];
  };
}

