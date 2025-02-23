{ config, self, lib, ... }:
let
  mkAppOpt = { description ? "", default ? null }: lib.mkOption {
    inherit description default;
    type = lib.types.coercedTo lib.types.package lib.getExe lib.types.str;
  };

  mkAppOpt' = default: mkAppOpt { inherit default; description = ""; };

  mkMakoOsd = category: ''
    [category=${category}]
    anchor=top-center
    margin=4,0,4,0
  '';
in
{
  # https://github.com/emersion/mako/blob/master/doc/mako.5.scd
  config = {
    services.mako.extraConfig = lib.mkIf config.services.mako.enable ''
      ${mkMakoOsd "brightness-osd"}
      ${mkMakoOsd "volume-osd"}
    '';
  };
}