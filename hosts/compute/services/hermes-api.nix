{ config, inputs, ... }:
{
  custom.homelab.services.hermes-api = {
    displayName = "Hermes";
    metadata.description = "Personal assistant";
    metadata.version = inputs.self.nixosConfigurations.personal-agent.config.services.hermes-agent.package.version;
    metadata.homepage = "https://hermes-agent.nousresearch.com/";
    metadata.category = "Infrastructure";

    host = config.custom.fleet.microvm.hosts.personal-agent;
    port = 8642;
    subdomain = "hermes-api";

    integrations.monitoring.enable = false;
    integrations.homepage.enable = false;
    # ntfy bridges Hermes ↔ phone:
    #   wo personal-agent       → outbound cron/reminders/agent replies
    #   ro personal-agent-inbox → subscribe loop receives user pushes
    # Single ntfy user (`hermes-api`), single token, two ACLs.
    # Token plaintext lands at /var/lib/homelab-secrets/ntfy-publishers/hermes-api
    # on compute; one-time copy into personal-agent's sops as
    # `hermes-agent/ntfy-publisher-token` (see hosts/personal-agent/default.nix).
    integrations.ntfy = {
      enable = true;
      topic = "personal-agent";
      access = "wo";
      extraAccess."personal-agent-inbox" = "ro";
    };
  };
}
