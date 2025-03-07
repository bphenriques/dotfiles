{ pkgs, lib, config, ... }:
let
  mkMakoOsd = category: ''
    [category=${category}]
    anchor=top-center
    margin=10,0,10,0
  '';
in
{
  stylix.targets.mako.enable = config.stylix.enable;
  services.mako = {
    enable = true;
    layer = "overlay";
    defaultTimeout = 5000;

    # Theming is covered separately.
    width = 300;
    height = 200;
    margin = "8";
    padding = "12";
    borderSize = 1;
    borderRadius = 4;

    extraConfig = ''
      [urgency=critical]
      default-timeout=0

      ${mkMakoOsd "brightness-osd"}
      ${mkMakoOsd "volume-osd"}
    '';
  };

  custom.programs.niri.layerRules.screencasting.block = [ ''namespace="^notifications$"'' ];
}