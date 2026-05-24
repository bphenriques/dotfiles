# Hermes API server exposed by the hermes-vm microvm.
#
# Phase 2 of the blue/green migration: traffic for `hermes-vm-api.{domain}`
# is proxied by compute's Traefik to the VM at 10.20.1.10:8642. Blue's
# hermes-agent on compute keeps serving `hermes-api.{domain}` until we
# cut over (renaming this entry's subdomain). See
# bphenriques-tools/microvm.md for the full sequence.
{ config, pkgs, self, ... }:
let
  serviceCfg = config.custom.homelab.services.hermes-vm-api;
  vmSecretsDir = "/var/lib/microvm-secrets/hermes-vm";
  # Resolve the VM's settings here on compute so the host can render
  # the YAML the VM would otherwise have to bootstrap by hand. JSON is
  # valid YAML; `services.hermes-agent.settings` already has mcp_servers
  # merged in (the module does that at evaluation time).
  vmConfigYaml = pkgs.writeText "hermes-vm-config.yaml"
    (builtins.toJSON self.nixosConfigurations.hermes-vm.config.services.hermes-agent.settings);
in
{
  custom.homelab.services.hermes-vm-api = {
    displayName = "Hermes (VM)";
    metadata.description = "Personal assistant — OpenAI-compatible API (microvm)";
    metadata.version = config.services.hermes-agent.package.version or "n/a";
    metadata.homepage = "https://hermes-agent.nousresearch.com/";
    metadata.category = "Infrastructure";

    # Backend lives in the VM, reached over compute's br-hermes bridge.
    host = "10.20.1.10";
    port = 8642;

    # Temporary subdomain — promoted to "hermes-api" at cut-over.
    subdomain = "hermes-vm-api";

    integrations.monitoring.enable = false;
    integrations.homepage.enable = false;

    secrets = {
      files.api-token = { rotatable = true; bytes = 32; };

      # Rendered to a directory shared into the VM via virtiofs. Mode 0644
      # because the VM's hermes user has a different GID than the host's
      # homelab-secrets-hermes-vm-api group — file readable across the
      # boundary by the only consumer inside the VM.
      templates."env" = {
        path = "${vmSecretsDir}/env";
        mode = "0644";
        content = ''
          API_SERVER_ENABLED=true
          API_SERVER_PORT=8642
          API_SERVER_HOST=0.0.0.0
          API_SERVER_KEY=${serviceCfg.secrets.placeholder.api-token}
        '';
      };
      # The consumer (hermes-agent) lives inside the VM, but the host
      # still needs the secrets rendered before the microvm starts —
      # the virtiofs share's source directory must exist. Wire the
      # microvm unit so the secrets job runs before VM boot.
      systemd.dependentServices = [ "microvm@hermes-vm" ];
    };
  };

  # Render config.yaml into the host-secrets share at boot. The VM's
  # hermes-agent unit has an `ExecStartPre` that installs it into
  # $HERMES_HOME/config.yaml — both files thus stay in lock-step with
  # the Nix declaration, no manual `cp` after rebuilds.
  # `C+` = copy, overwriting if it already exists, so re-evaluating
  # settings + rebuild always refreshes the host-side file.
  systemd.tmpfiles.rules = [
    "C+ ${vmSecretsDir}/config.yaml 0644 root root - ${vmConfigYaml}"
  ];
}
