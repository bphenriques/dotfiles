{ ... }:
{
  # Wiring (ingress, auth, API key) is the framework's; the indexer sync + app connections stay here
  # (./configure.nix), reading indexers from the private flake and the *arr keys from apps.*.apiKeyFile.
  imports = [ ./configure.nix ];

  selfhost.apps.prowlarr.enable = true;
  selfhost.services.prowlarr.integrations.notify.topic = "admin";
}
