_:{
  networking.firewall = {
    allowedTCPPorts = [ 80 443 ];
    allowedUDPPorts = [ 443 ];
  };

  services.traefik = {
    enable = true;

        # Ensure Traefik can access the Docker socket
        # group = "docker";



    staticConfigOptions = {
      log.level = "ERROR";

      # EntryPoints
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
            # Wildcard Domains
            domains = [
              {
                main = "example.com";     # Replace with your ${HOME_SERVER_CNAME}
                sans = [ "*.example.com" ];
              }
            ];
          };

          # Transport Timeouts.
          transport.respondingTimeouts = {
            readTimeout = "600s";   # Defaults to 60s which seems too short to upload big files.
            writeTimeout = "600s";  # Defaults to no timeout. Restricting (same guide as immich).
          };
        };
      };

      # Providers
      providers = {
        docker = {
          # Connect directly to socket (no proxy needed on host)
          endpoint = "unix:///var/run/docker.sock";
          exposedByDefault = false;
        };
      };

      # Certificate Resolvers (ACME)
      certificatesResolvers = {
        default = {
          acme = {
            # Replace with your ${HOME_SERVER_ACME_EMAIL}
            email = "your-email@example.com";
            storage = "/var/lib/traefik/acme.json"; # Standard NixOS state directory
            dnsChallenge = {
              provider = "cloudflare";
              # Resolvers are sometimes needed if the host DNS is restrictive
              # resolvers = [ "1.1.1.1:53" "8.8.8.8:53" ];
            };
          };
        };
      };
    };

    # Create a file at /var/secrets/traefik.env (chmod 600) with content:
    # CF_DNS_API_TOKEN=your_real_token_here
    # CF_API_EMAIL=your_email_here
    systemd.services.traefik.serviceConfig = {
      EnvironmentFile = "/var/secrets/traefik.env";
    };
  };
}
