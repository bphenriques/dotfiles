{ config, lib, self, pkgs, ... }:
let
  cfg = config.custom.programs.wlr-which-key;
  yamlFormat = pkgs.formats.yaml { };
in
{
  options.custom.programs.wlr-which-key = {
    enable = lib.mkEnableOption "wlr-which-key";
    package = lib.mkOption {
      type = lib.types.package;
      default = self.pkgs.wlr-which-key;
    };
    settings = lib.mkOption {
      type = yamlFormat.type;
      default = { };
    };
    menus = lib.mkOption {
      type = lib.types.attrsOf yamlFormat.type;
      default = [ ];
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = [ (lib.hm.assertions.assertPlatform "custom.programs.wlr-which-key" pkgs lib.platforms.linux) ];
    home.packages = [ cfg.package ];

    xdg.configFile = lib.attrsets.mapAttrs' (
      name: menu: lib.attrsets.nameValuePair "wlr-which-key/${name}.yaml" {
        text = builtins.toJSON (cfg.settings // { inherit menu; }); # toJSON as YAML breaks (especially with nix paths)
      }
    ) cfg.menus;
  };
}