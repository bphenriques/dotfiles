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
    # No homelab-secrets template here anymore — the api-server-key
    # lives in sops (shared with hermes-vm), so we render the env file
    # via sops.templates below instead.
  };

  # Decrypt the VM's API token on compute (multi-recipient sops file
  # has both compute's and hermes-vm's age keys listed).
  sops.secrets."hermes-agent/api-server-key" = {
    owner = "root";
    mode = "0440";
  };
  # NextChat env file — sops renders the placeholder at decryption time.
  sops.templates."nextchat-env" = {
    content = ''
      OPENAI_API_KEY=${config.sops.placeholder."hermes-agent/api-server-key"}
    '';
    owner = "root";
    mode = "0440";
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

      # Hermes lives in the microvm now. NextChat reaches it via the
      # compute-microvm bridge (no loopback), no Traefik / TLS in the way.
      BASE_URL = "http://${hermesApiCfg.host}:${toString hermesApiCfg.port}";
      CUSTOM_MODELS = "-all,+hermes-agent";
      DEFAULT_MODEL = "hermes-agent";

      # Minimal/locked-down UX — single backend, no per-user overrides.
      HIDE_USER_API_KEY = "1";
      HIDE_BALANCE_QUERY = "1";  # Hermes has no balance endpoint
      DISABLE_FAST_LINK = "1";
      DISABLE_GPT4 = "1";  # we don't use it; removes UI noise + provider switches
      WHITE_WEBDAV_ENDPOINTS = "";  # disable third-party WebDAV sync
    };

    environmentFiles = [ config.sops.templates."nextchat-env".path ];

    extraOptions = [
      "--network=host"  # reach Hermes API on the compute-microvm bridge (10.20.1.x)
      # 1G — 512m was tight; Next.js standalone can OOM-restart under sustained sessions.
      "--memory=1g"
    ];
  };
}
