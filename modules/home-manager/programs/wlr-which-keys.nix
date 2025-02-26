{ config, lib, pkgs, ... }:
let
  cfg = config.custom.programs.wlr-which-key;
  yamlFormat = pkgs.formats.yaml { };
in
{
  options.custom.programs.wlr-which-key = {
    enable = lib.mkEnableOption "wlr-which-key";
    settings = lib.mkOption {
      type = yamlFormat.type;
      default = { };
    };
    menus = lib.mkOption {
      type = lib.types.attrsOf yamlFormat.type;
      default = { };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.wlr-which-key" pkgs lib.platforms.linux) ];
    home.packages = [ pkgs.wlr-which-key ];

    xdg.configFile = lib.attrsets.mapAttrs' (
      name: menu: lib.attrsets.nameValuePair "wlr-which-key/${name}.yaml" {
        source = yamlFormat.generate "${name}.yaml" (cfg.settings // { inherit menu; });
      }
    ) cfg.menus;
  };
}