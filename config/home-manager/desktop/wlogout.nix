{ pkgs, lib, ... }:
{
  # Theme? https://github.com/Sum1Code/nixos-dotfile/blob/ce81aab23096e65ea7448a060ddf41b0bb627167/home/apps/wlogout/style.nix
  programs.wlogout = {
    enable = true;
    layout = [
      {
        label = "lock";
        action = "hyprlock";
        text = "Lock";
        keybind = "l";
      }
      {
    	  label = "logout";
    	  action = "hyprctl dispatch exit 0";
    	  text = "Logout";
    	  keybind = "e";
      }
      {
    	  label = "shutdown";
    	  action = "systemctl poweroff";
    	  text = "Shutdown";
    	  keybind = "s";
      }
      {
    	  label = "suspend";
    	  action = "systemctl suspend";
    	  text = "Suspend";
    	  keybind = "u";
      }
      {
    	  label = "reboot";
    	  action = "systemctl reboot";
    	  text = "Reboot";
    	  keybind = "r";
      }
    ];
  };
}