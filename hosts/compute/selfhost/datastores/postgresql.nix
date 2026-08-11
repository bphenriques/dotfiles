# Services: Miniflux, Immich
{ lib, pkgs, config, ... }:
{
  assertions = [{
    assertion = lib.versionOlder config.services.postgresql.package.version "17";
    message = "PostgreSQL must be < 17 (Immich + pgvecto.rs constraint). Got: ${config.services.postgresql.package.version}";
  }];

  services.postgresql = {
    enable = true;
    package = pkgs.postgresql_16; # Immich + pgvecto.rs requires <= 16
  };

  # After an unclean shutdown the stale postmaster.pid can name a PID a boot-time transient
  # now holds, and postgres refuses to start until that PID exits. Upstream sizes the start
  # limit for 5 slow attempts but never sets RestartSec, so instant failures burn all 5 in
  # under a second. Spreading them over a minute lets the colliding PID clear.
  # Not an ExecStartPre that deletes the pid file: it cannot distinguish a stale lock from a
  # live postmaster, so it would trade a self-healing outage for silent data corruption.
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
