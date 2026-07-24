{
  _module.args.agentVm = {
    stateRoot = "/var/lib/hermes";
    apiPort = 8642;
    vaultRoot = "/var/lib/vault";           # Readonly from compute
    secretsRoot = "/var/lib/agent-secrets"; # Readonly from compute
  };
}
