{ config, inputs, ... }:
{
  custom.homelab.services.hermes-api = {
    displayName = "Hermes";
    metadata.description = "Personal assistant";
    # Cross-flake lookup: the hermes-agent module is enabled on the VM, not compute.
    metadata.version = inputs.self.nixosConfigurations.personal-agent.config.services.hermes-agent.package.version;
    metadata.homepage = "https://hermes-agent.nousresearch.com/";
    metadata.category = "Infrastructure";

    host = config.custom.fleet.microvm.hosts.personal-agent;
    port = 8642;
    subdomain = "hermes-api";

    integrations.monitoring.enable = false;
    integrations.homepage.enable = false;
  };
}
