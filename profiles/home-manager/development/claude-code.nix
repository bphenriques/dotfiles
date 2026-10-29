{ config, ... }:
{
  programs.claude-code.enable = true;
  programs.claude-code.configDir = "${config.xdg.configHome}/claude";
}
