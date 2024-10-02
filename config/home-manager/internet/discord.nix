{ config, pkgs, lib, ... }:
{
  home.packages = with pkgs; lib.optionals (pkgs.stdenv.isLinux) [ discord ];
  systemd.user.tmpfiles.rules = lib.optionalString (pkgs.stdenv.isLinux) [
    "d ${config.xdg.configHome}/discord/Cache/Cache_Data - - - 10d" # Clean cache older than 10 days
  ];
}
