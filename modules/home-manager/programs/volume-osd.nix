{ lib, pkgs, config, self, ... }:
let
  cfg = config.custom.programs.volume-osd;

  cmd = desc: cmd: { inherit desc cmd; };
  cmdKeepOpen = desc: cmd: { inherit desc cmd; keep_open = true; };

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
    custom.programs.wlr-which-key.menus.volume-osd = {
      c = cmd "Configure" (lib.getExe pkgs.pavucontrol);
      m = cmdKeepOpen "Mute" "${volume-osd} sink-toggle-mute";
      n = cmdKeepOpen "Next audio sink" "${volume-osd} sink-move-next";
      p = cmdKeepOpen "Previous audio sink" "${volume-osd} sink-move-prev";
      s = cmd "Select output" "${volume-osd} sink-move-fuzzel";
    };

    home.packages = [
      cfg.package
      pkgs.pavucontrol  # UI
      pkgs.pulseaudio   # Command line
    ];
  };
}