{ ... }:
{
  imports = [ ./configure.nix ];

  selfhost.apps.prowlarr.enable = true;
  selfhost.services.prowlarr.integrations.notify.topic = "admin";
}
