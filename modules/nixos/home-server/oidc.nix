{ lib, config, ... }:
let
  homeServerCfg = config.custom.home-server;
  cfg = homeServerCfg.oidc;

  clientOpt = lib.types.submodule ({ name, ... }: {
    options = {
      name = lib.mkOption {
        type = lib.types.str;
        default = name;
        description = "Name of the OIDC client in Pocket-ID";
      };

      callbackURLs = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ "${homeServerCfg.routes.${name}.publicUrl}/oauth2/oidc/callback" ];
        description = "Callback URLs for the OIDC client";
      };

      pkce = lib.mkOption { type = lib.types.bool; default = false; };

      credentialsSecret = lib.mkOption {
        type = lib.types.str;
        default = "pocket-id/oidc-clients/${name}";
        description = ''
          Name of a sops-nix secret containing the OIDC client credentials
          in env-file format (OAUTH2_CLIENT_ID=..., OAUTH2_CLIENT_SECRET=...).
        '';
      };

      # Wiring options
      systemd = {
        enable = lib.mkEnableOption "Inject OIDC credentials into systemd service";
        service = lib.mkOption {
          type = lib.types.str;
          default = name;
          description = "Systemd unit name to inject OIDC credentials into";
        };
      };

      podman = {
        enable = lib.mkEnableOption "Inject OIDC credentials into podman container";
        container = lib.mkOption {
          type = lib.types.str;
          default = name;
          description = "Podman container name to inject OIDC credentials into";
        };
      };
    };
  });

  getSecretPath = clientName: config.sops.secrets.${cfg.clients.${clientName}.credentialsSecret}.path;
in
{
  options.custom.home-server.oidc = {
    discoveryEndpoint = lib.mkOption {
      type = lib.types.str;
      default = homeServerCfg.routes.pocket-id.publicUrl;
      description = "OIDC discovery endpoint URL";
    };

    clients = lib.mkOption {
      type = lib.types.attrsOf clientOpt;
      default = { };
      description = "OIDC clients to register with the provider";
    };
  };

  config = {
    # Inject into systemd services
    systemd.services = lib.mapAttrs' (_: clientCfg:
      lib.nameValuePair clientCfg.systemd.service {
        serviceConfig.EnvironmentFile = [ (getSecretPath clientCfg.name) ];
      }
    ) (lib.filterAttrs (_: c: c.systemd.enable) cfg.clients);

    # Inject into podman containers
    virtualisation.oci-containers.containers = lib.mapAttrs' (_: clientCfg:
      lib.nameValuePair clientCfg.podman.container {
        environmentFiles = [ (getSecretPath clientCfg.name) ];
      }
    ) (lib.filterAttrs (_: c: c.podman.enable) cfg.clients);
  };
}
