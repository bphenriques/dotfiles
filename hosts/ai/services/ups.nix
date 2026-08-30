{ config, ... }:
{
  # NUT client. The server is storage; credentials are its `ups/ai-password`.
  power.ups = {
    enable = true;
    mode = "netclient";
    upsmon.monitor.storage = {
      system = "storage@${config.custom.fleet.lan.hosts.storage}";
      powerValue = 1;
      user = "ai";
      passwordFile = config.sops.secrets."upsmon/password".path;
      type = "secondary";
    };
  };

  sops.secrets."upsmon/password" = { };
}
