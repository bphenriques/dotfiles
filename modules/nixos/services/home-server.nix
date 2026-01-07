{ lib, config, ... }:
let
  cfg = config.custom.home-server;

  serviceOpt = lib.types.submodule ({ name, config, ... }: {
    options = {
      name = lib.mkOption { type = lib.types.str; default = name; };
      internalUrl  = lib.mkOption { type = lib.types.str; };
      subdomain = lib.mkOption { type = lib.types.str; default = name; };
      host = lib.mkOption { type = lib.types.str; default = "${config.subdomain}.${cfg.domain}"; };
      publicUrl = lib.mkOption { type = lib.types.str; default = "https://${config.host}"; };

      # TODO actually use
      oidc = {
        enable = lib.mkEnableOption "OIDC registration";
        callbackPath = lib.mkOption {
          type = lib.types.str;
          default = "/oauth2/oidc/callback";
        };
      };
    };
  });

  mkTraefikService = service: {
    http = {
      routers."${service.name}" = {
        rule = "Host(`${service.host}`)";
        entryPoints = [ "websecure" ];
        service = "${service.name}-svc";
      };
      services."${service.name}-svc".loadBalancer.servers = [{ url = service.internalUrl; }];
    };
  };
in
{
  options.custom.home-server = {
    enable = lib.mkEnableOption "Home-server service";
    domain = lib.mkOption {
      type = lib.types.str;
    };

    cloudflareEmail = lib.mkOption {
      type = lib.types.str;
    };

    services = lib.mkOption {
      type = lib.types.attrsOf serviceOpt;
      default = { };
    };
  };

  config = lib.mkIf cfg.enable {
    assertions = let
       urls = lib.mapAttrsToList (_: v: v.internalUrl) cfg.services;
       duplicates = lib.subtractLists urls (lib.unique urls);
     in [
      {
        assertion = (builtins.length duplicates) == 0;
        message = "Service URLs must be unique to avoid routing conflicts. Conflicting: ${toString duplicates}";
      }
    ];

    networking.firewall = {
      allowedTCPPorts = [ 80 443 ];
      allowedUDPPorts = [ 443 ];
    };

    sops = {
      secrets.cloudflare_dns_api_token = { };
      templates.home-server = {
        content = ''
          CF_DNS_API_TOKEN=${config.sops.placeholder.cloudflare_dns_api_token}
          CF_API_EMAIL=${cfg.cloudflareEmail}
        '';
      };
    };

    systemd.services.traefik.serviceConfig = {
      EnvironmentFile = config.sops.templates.home-server.path;
    };

    services.traefik = {
      enable = true;
      staticConfigOptions = {
        log.level = "ERROR";
        entryPoints = {
          web = {
            address = ":80";
            http.redirections.entryPoint = {
              to = "websecure";
              scheme = "https";
            };
          };

          websecure = {
            address = ":443";
            http.tls = {
              certResolver = "default";
              domains = [{
                main = cfg.domain;
                sans = [ "*.${cfg.domain}" ];
              }];
            };

            # Set some sensible defaults
            transport.respondingTimeouts = {
              readTimeout = "300s";   # To upload large assets.
              writeTimeout = "300s";  # To download large assets.
            };
          };
        };

        #providers = {
        #  docker = {
        #    # Connect directly to socket (no proxy needed on host)
        #    endpoint = "unix:///var/run/docker.sock";
        #    exposedByDefault = false;
        #  };
        #};

        certificatesResolvers.default.acme = {
          email = cfg.cloudflareEmail;
          storage = "/var/lib/traefik/acme.json"; # Standard NixOS state directory
          dnsChallenge.provider = "cloudflare";
        };
      };

      dynamicConfigOptions = let
        serviceConfigs = lib.mapAttrsToList (name: value: mkTraefikService value) cfg.services;
      in lib.foldl' lib.recursiveUpdate {} serviceConfigs;
    };
  };
}