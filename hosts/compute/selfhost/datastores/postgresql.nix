# Services: Miniflux, Immich, RomM
{ pkgs, config, ... }:
{
  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_18;
  };

  # Spread restarts over a minute so a stale postmaster.pid colliding with a boot-time PID can clear. Never delete the pid file: it can't tell stale from live.
  systemd.services.postgresql.serviceConfig.RestartSec = 15;

  selfhost.monitoring.scopes.postgres = {
    exporters.postgres = {
      enable = true;
      listenAddress = "127.0.0.1";
      port = 9187;
      runAsLocalSuperUser = true;
    };
    scrapeConfigs = [{
      job_name = "postgres";
      scrape_interval = "120s";
      static_configs = [{
        targets = [ "127.0.0.1:9187" ];
        labels.instance = config.networking.hostName;
      }];
    }];
    rules = [{
      name = "postgres";
      rules = [
        {
          alert = "PostgresDown";
          expr = "pg_up == 0";
          "for" = "5m";
          labels.severity = "critical";
          annotations.summary = "PostgreSQL down";
        }
        {
          alert = "PostgresHighConnections";
          expr = "sum by(instance) (pg_stat_activity_count) > 80";
          "for" = "5m";
          labels.severity = "warning";
          annotations.summary = "PostgreSQL > 80 connections";
        }
      ];
    }];
  };
}
