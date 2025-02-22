{ pkgs, config, lib, ... }:
{
  stylix.targets.mako.enable = true;
  services.mako = {
    enable = true;
    layer = "overlay";
    defaultTimeout = 5000;

    # Theming is covered by stylix
    width = 300;
    height = 200;
    margin = "8";
    padding = "12";
    borderSize = 1;
    borderRadius = 4;

    extraConfig = ''
      [urgency=critical]
      default-timeout=0
    '';
  };
}