{
  stateRoot = "/var/lib/hermes";
  apiPort = 8642;
  webuiPort = 8787;

  # Read only
  secretsRoot = "/var/lib/agent-secrets"; # Readonly from compute

  # R/W
  vaultRoot = "/var/lib/vault";
  recipesRoot = "/var/lib/recipes";
}
