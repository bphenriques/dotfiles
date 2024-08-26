{ config, pkgs, ... }:
{
  home.packages = with pkgs; [ discord ];

  custom.impermanence.discord = true;
  systemd.user.tmpfiles.rules = [
    "d ${config.xdg.configHome}/discord/Cache/Cache_Data - - - 10d" # Clean cache older than 10 days
  ];
}
