{ config, lib, pkgs, ... }:

{
  # Requires:
  # - iproute2
  # - Manually setting up Desktop shortcuts (TODO: automate it)
  # - Manually setting up change language shortcut (TODO: automate it)
  # - Nerd Font
  xdg.configFile = {
    "sketchybar/sketchybarrc".source = ./config/sketchybarrc.sh;
    "sketchybar/theme.sh".source = ./config/theme.sh;
    "sketchybar/plugins".source = ./config/plugins;
  };
}
