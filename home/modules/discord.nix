{ config, lib, pkgs, ... }:
let
  cfg = config.modules.programs.discord;
  jsonFormat = pkgs.formats.json { };
in
{
  options.modules.programs.discord = with lib; {
    enable = mkEnableOption "discord";
  };

  config = lib.mkIf cfg.enable {
    home.packages = with pkgs; [ discord ];
    xdg.configFile."discord/settings.json".source =
      jsonFormat.generate "discord.json" {
        SKIP_HOST_UPDATE = true;              # Discord crashes if it is not up to date.
      };
  };
}
