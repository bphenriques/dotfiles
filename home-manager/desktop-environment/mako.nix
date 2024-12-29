{ pkgs, lib, ... }:
{
  services.mako = {
    enable = true;
    layer = "overlay";
    defaultTimeout = 5000;

    # Theming
    backgroundColor = "#151515";
    textColor = "#e8e3e3";
    progressColor = "over #2e2e2e";
    width = 300;
    height = 200;
    margin = "8";
    padding = "12";
    borderSize = 1;
    borderRadius = 4;
    borderColor= "#424242";

    extraConfig = ''
    [urgency=critical]
    border-color=#b66467
    default-timeout=0
    '';
  };
}