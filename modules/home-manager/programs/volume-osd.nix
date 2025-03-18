{ lib, pkgs, config, self, ... }:
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
        errorIcon = mkIcon "volume-osd-error-icon" "";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.custom-volume-osd" pkgs lib.platforms.linux) ];
    custom.programs.wlr-which-key.menus.volume-osd = [
      { key = ["Left" "h"];     desc = "Previous audio sink"; cmd = "${volume-osd} sink-move-prev"; keep_open = true; }
      { key = ["Right" "l"];    desc = "Next audio sink";     cmd = "${volume-osd} sink-move-next"; keep_open = true; }
      { key = "space";          desc = "Select output";       cmd = "${volume-osd} sink-move-fuzzel"; }
      { key = "m";              desc = "Mute";                cmd = "${volume-osd} sink-toggle-mute"; keep_open = true; }
      { key = "c";              desc = "Configure";           cmd = (lib.getExe pkgs.pavucontrol); }
    ];

    home.packages = [
      cfg.package
      pkgs.pavucontrol  # UI
      pkgs.pulseaudio   # Command line
    ];
  };
}

