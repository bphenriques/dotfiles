# Generate agent-vm's credentials here and expose only their rendered env files over RO virtiofs. Compute is
# the guest's hypervisor, so no sops/ssh needed. One file per credential: the API key is shared with the chat
# UI, the web UI password is not shared with anything.
{ config, ... }:
{
  selfhost.runtimeSecrets."hermes-api-server-key" = { }; # openssl rand -hex 32

  # restartUnits orders the VM after the render and reboots it on key rotation.
  selfhost.runtimeTemplates."agent-vm-hermes-env" = {
    content = "API_SERVER_KEY=${config.selfhost.runtimePlaceholder."hermes-api-server-key"}\n";
    path = "/var/lib/agent-vm-secrets/hermes.env";
    restartUnits = [ "microvm@agent-vm.service" ];
  };

  selfhost.runtimeSecrets."hermes-webui-password" = { };

  selfhost.runtimeTemplates."agent-vm-webui-env" = {
    content = "HERMES_WEBUI_PASSWORD=${config.selfhost.runtimePlaceholder."hermes-webui-password"}\n";
    path = "/var/lib/agent-vm-secrets/webui.env";
    restartUnits = [ "microvm@agent-vm.service" ];
  };
}
