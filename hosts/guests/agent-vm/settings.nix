# Guest-owned facts. Imported by the guest's modules and by compute, which fronts its API.
{
  stateRoot = "/var/lib/hermes";
  apiPort = 8642;
  vaultRoot = "/var/lib/vault";           # The live vault, RW over virtiofs from compute
  secretsRoot = "/var/lib/agent-secrets"; # Readonly from compute
}
