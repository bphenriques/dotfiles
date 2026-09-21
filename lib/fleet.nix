{ lib, nixosConfigurations, producers }:
let
  producerCfgs = lib.getAttrs producers nixosConfigurations;
  fromHost = _: cfg:
    lib.concatLists (
      lib.mapAttrsToList (
        name: s:
        lib.optional (s.extraConfig.landingPage.enable or false) {
          inherit name;
          inherit (s) displayName;
          inherit (s.meta) homepage;
          category = if s.meta.category == null then "other" else s.meta.category;
          order = s.extraConfig.landingPage.order or 1;
          listed = s.extraConfig.landingPage.listed or true;
        }
      ) cfg.config.selfhost.services
    );
in
{
  hosts = builtins.length (builtins.attrNames nixosConfigurations);
  services = lib.concatLists (lib.mapAttrsToList fromHost producerCfgs);
}
