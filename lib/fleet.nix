# Derived fleet facts (host count + public landing-page service list) for whichever host consumes them.
# A service opts into the page with `extraConfig.landingPage.enable`; its facts (displayName, homepage,
# category) come from selfhost's use-case-agnostic inventory, and `order` is the landing-specific sort.
# `producers` is an allowlist of selfhost host names: consumers are never in it, so these facts can be fed
# back into a consumer's config without a fixpoint cycle.
{ lib, nixosConfigurations, producers }:
let
  producerCfgs = lib.getAttrs producers nixosConfigurations;
  fromHost =
    _: cfg:
    let
      byName = builtins.listToAttrs (
        map (i: { inherit (i) name; value = i; }) (cfg.config.selfhost.inventory or [ ])
      );
    in
    lib.concatLists (
      lib.mapAttrsToList (
        name: s:
        lib.optional (s.extraConfig.landingPage.enable or false) (
          let
            fact = byName.${name} or { };
          in
          {
            inherit name;
            displayName = fact.displayName or name;
            homepage = fact.homepage or null;
            category = fact.category or "other";
            order = s.extraConfig.landingPage.order or 1;
          }
        )
      ) (cfg.config.selfhost.services or { })
    );
in
{
  hosts = builtins.length (builtins.attrNames nixosConfigurations);
  services = lib.concatLists (lib.mapAttrsToList fromHost producerCfgs);
}
