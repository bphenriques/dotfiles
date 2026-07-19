# Derived fleet facts for guests that render them (cv-vm landing page + curl headers).
# attrNames is spine-only, so reading these never forces the guest configs (no recursion).
{ nixosConfigurations }:
{
  hosts = builtins.length (builtins.attrNames nixosConfigurations);
  services = builtins.length (builtins.attrNames nixosConfigurations.compute.config.selfhost.services);
}
