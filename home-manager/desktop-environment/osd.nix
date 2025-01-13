{ config, self, ... }:

let
  mkMakoOsd = category: ''
    [category=${category}]
    anchor=top-center
    margin=4,0,4,0
  '';
in
{

  home.packages = [
    self.pkgs.osd-brightness
    self.pkgs.osd-volume
  ];

  # https://github.com/emersion/mako/blob/master/doc/mako.5.scd
  services.mako.extraConfig = ''
    ${mkMakoOsd "brightness-osd"}
    ${mkMakoOsd "volume-osd"}
  '';
}