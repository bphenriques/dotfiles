{ config, agentVm, inputs, ... }:
let
  # The agent's module derives HERMES_HOME as `${stateDir}/.hermes`, and that state is the whole point of
  # this front end: pointed anywhere else it comes up as a second agent with empty memory and no config.
  hermesHome = "${config.services.hermes-agent.stateDir}/.hermes";
in
{
  imports = [ inputs.hermes-webui.nixosModules.default ];

  services.hermes-webui = {
    enable = true;

    # Runs as the agent itself: HERMES_HOME is 0700 hermes, and this is one front end over that state
    # rather than a second identity.
    user = "hermes";
    group = "hermes";
    inherit hermesHome;
    stateDir = "${hermesHome}/webui";   # upstream's own layout, and on the volume: the module's default is not

    host = "0.0.0.0";   # the gateway-only rule is the gate here too, so openFirewall stays off
    port = agentVm.webuiPort;

    agent.package = config.services.hermes-agent.package;       # HERMES_WEBUI_PYTHON, from its passthru.hermesVenv
    environmentFiles = [ "${agentVm.secretsRoot}/webui.env" ];  # HERMES_WEBUI_PASSWORD, generated on compute
  };

  # Same reason as hermes-agent: the env file arrives over virtiofs, which is not mounted at activation.
  systemd.services.hermes-webui = {
    unitConfig.RequiresMountsFor = [ agentVm.secretsRoot ];

    # Upstream's unit sets no sandbox at all. ProtectSystem is left out on purpose: it would need the
    # agent's home and the vault listed back as writable, which is the whole of what this serves.
    serviceConfig = {
      NoNewPrivileges = true;
      PrivateTmp = true;
      ProtectKernelTunables = true;
      ProtectControlGroups = true;
      RestrictSUIDSGID = true;
    };
  };
}
