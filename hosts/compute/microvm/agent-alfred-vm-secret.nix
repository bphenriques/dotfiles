# Generate agent-alfred-vm's API key here and expose only its rendered env file over RO virtiofs. Compute is
# the guest's hypervisor, so no sops/ssh needed. The key is shared with the chat UI.
{ config, ... }:
{
  selfhost.runtimeSecrets."hermes-api-server-key" = { }; # openssl rand -hex 32

  # restartUnits orders the VM after the render and reboots it on key rotation.
  selfhost.runtimeTemplates."agent-alfred-vm-hermes-env" = {
    content = "API_SERVER_KEY=${config.selfhost.runtimePlaceholder."hermes-api-server-key"}\n";
    path = "/var/lib/agent-alfred-vm-secrets/hermes.env";
    restartUnits = [ "microvm@agent-alfred-vm.service" ];
  };
}
