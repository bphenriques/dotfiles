# Generate agent-alfred-vm's credentials here and expose only their rendered env files over RO virtiofs. Compute is
# the guest's hypervisor, so no sops/ssh needed. One file per credential: the API key is shared with the chat
# UI, the web UI password is not shared with anything.
{ config, lib, ... }:
let
  cfg = config.selfhost;
  webui = cfg.services.hermes-webui;
in
{
  selfhost.runtimeSecrets."hermes-api-server-key" = { }; # openssl rand -hex 32

  # restartUnits orders the VM after the render and reboots it on key rotation.
  selfhost.runtimeTemplates."agent-alfred-vm-hermes-env" = {
    content = "API_SERVER_KEY=${config.selfhost.runtimePlaceholder."hermes-api-server-key"}\n";
    path = "/var/lib/agent-alfred-vm-secrets/hermes.env";
    restartUnits = [ "microvm@agent-alfred-vm.service" ];
  };

  # Pocket ID is the gate, so the web UI holds no password of its own. The redirect comes from the route's
  # own client registration rather than being spelled twice.
  selfhost.runtimeTemplates."agent-alfred-vm-webui-env" = {
    content = ''
      HERMES_WEBUI_OIDC_ISSUER=${cfg.auth.oidc.provider.issuerUrl}
      HERMES_WEBUI_OIDC_CLIENT_ID=${cfg.oidcPlaceholder.hermes-webui.id}
      HERMES_WEBUI_OIDC_CLIENT_SECRET=${cfg.oidcPlaceholder.hermes-webui.secret}
      HERMES_WEBUI_OIDC_REDIRECT_URI=${lib.head webui.access.oidc.callbackURLs}
    '';
    path = "/var/lib/agent-alfred-vm-secrets/webui.env";
    restartUnits = [ "microvm@agent-alfred-vm.service" ];
  };
}
