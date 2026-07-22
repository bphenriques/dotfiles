{ lib, nixosConfigurations, producers }:
let
  producerCfgs = lib.getAttrs producers nixosConfigurations;
  fromHost = _: cfg:
    let
      byName = builtins.listToAttrs (map (i: { inherit (i) name; value = i; }) cfg.config.selfhost.inventory);
    in
    lib.concatLists (
      lib.mapAttrsToList (
        name: s:
        lib.optional (s.extraConfig.landingPage.enable or false) (
          let
            fact = byName.${name};
          in
          {
            inherit name;
            inherit (fact) displayName homepage;
            category = if fact.category == null then "other" else fact.category;
            order = s.extraConfig.landingPage.order or 1;
            listed = s.extraConfig.landingPage.listed or true;
          }
        )
      ) cfg.config.selfhost.services
    );
in
{
  hosts = builtins.length (builtins.attrNames nixosConfigurations);
  services = lib.concatLists (lib.mapAttrsToList fromHost producerCfgs);
}
