# Derived fleet facts for cv-vm: host count + the public landing-page service list. A service opts into
# the page with `extraConfig.landingPage.enable`; its facts (displayName, homepage, category) come from
# selfhost's use-case-agnostic inventory, and `order` is the landing-specific sort. Excludes cv-vm (the
# consumer) to avoid a fixpoint cycle.
{ lib, nixosConfigurations }:
let
  producers = builtins.removeAttrs nixosConfigurations [ "cv-vm" ];
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
  services = lib.concatLists (lib.mapAttrsToList fromHost producers);
}
