{ config, pkgs, ... }:
let
  cfg = config.custom.homelab;
  serviceCfg = config.custom.homelab.services.nextchat;
  hermesApiCfg = config.custom.homelab.services.hermes-api;
  img = pkgs.containerImages.nextchat;
in
{
  # Friendly chat surface fronting the Hermes API. The Hermes built-in
  # dashboard at hermes.{domain} keeps the admin/TUI view.
  custom.homelab.services.nextchat = {
    displayName = "Chat";
    metadata.description = "Chat UI for the Hermes assistant";
    metadata.version = img.version;
    metadata.homepage = img.homepage;
    metadata.category = "Productivity";
    port = 3210;
    subdomain = "chat";
    access.allowedGroups = with cfg.groups; [ admin ];
    forwardAuth.enable = true;
    integrations.homepage.enable = true;
    secrets = {
      # OPENAI_API_KEY = bearer token belonging to the hermes-api service.
      # Cross-service placeholder: the framework detects the dependency
      # automatically and orders homelab-secrets-hermes-api before
      # homelab-secrets-nextchat.
      templates."env".content = ''
        OPENAI_API_KEY=${hermesApiCfg.secrets.placeholder.api-token}
      '';
      systemd.dependentServices = [ "podman-nextchat" ];
    };
  };

  virtualisation.oci-containers.containers.nextchat = {
    image = "${img.image}:v${img.version}";
    autoStart = true;

    environment = {
      # Next.js standalone honours PORT + HOSTNAME. With --network=host,
      # HOSTNAME defaults to the host's hostname (resolves to 127.0.0.2 via
      # /etc/hosts), which causes Traefik (pointing at 127.0.0.1:port) to
      # bad-gateway. Pin to 127.0.0.1 explicitly.
      HOSTNAME = "127.0.0.1";
      PORT = toString serviceCfg.port;

      # Hermes as the OpenAI-compatible backend on host loopback.
      BASE_URL = "http://127.0.0.1:${toString hermesApiCfg.port}";
      CUSTOM_MODELS = "-all,+hermes-agent";
      DEFAULT_MODEL = "hermes-agent";

      # Minimal/locked-down UX — single backend, no per-user overrides.
      HIDE_USER_API_KEY = "1";
      HIDE_BALANCE_QUERY = "1";  # Hermes has no balance endpoint
      DISABLE_FAST_LINK = "1";
      DISABLE_GPT4 = "1";  # we don't use it; removes UI noise + provider switches
      WHITE_WEBDAV_ENDPOINTS = "";  # disable third-party WebDAV sync
    };

    environmentFiles = [ serviceCfg.secrets.templates.env.path ];

    extraOptions = [
      "--network=host"  # reach Hermes API on compute's 127.0.0.1
      # 1G — 512m was tight; Next.js standalone can OOM-restart under sustained sessions.
      "--memory=1g"
    ];
  };
}
