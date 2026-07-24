# Generate the shared hermes API key here (NextChat + hermes both need it) and expose only its rendered
# env file to agent-vm over RO virtiofs. Compute is the guest's hypervisor, so no sops/ssh needed.
{ config, ... }:
{
  selfhost.runtimeSecrets."hermes-api-server-key" = { }; # openssl rand -hex 32

  # restartUnits orders the VM after the render and reboots it on key rotation.
  selfhost.runtimeTemplates."agent-vm-hermes-env" = {
    content = "API_SERVER_KEY=${config.selfhost.runtimePlaceholder."hermes-api-server-key"}\n";
    path = "/var/lib/agent-vm-secrets/hermes.env";
    restartUnits = [ "microvm@agent-vm.service" ];
  };
}
