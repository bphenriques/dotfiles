{ config, lib, ... }:
let
  cfg = config.selfhost;
  serviceCfg = cfg.services.chat;
  inherit (config.custom.fleet) ai;
  oidcCfg = cfg.auth.oidc;
  agentVmIp = config.custom.fleet.microvms.compute.agent-vm;

  hermesModel = "hermes-agent";

  # URLs and keys are paired by position, so both lists come from this one.
  backends = [
    {
      url = "http://${ai.endpoint.host}:${toString ai.endpoint.port}/v1";
      key = "ollama"; # dummy; Ollama needs no auth
      model_ids = [ ai.model ] ++ ai.extraModels;
    }
    {
      url = "http://${agentVmIp}:8642/v1";
      key = cfg.runtimePlaceholder."hermes-api-server-key";
      model_ids = [ hermesModel ]; # hermes re-exports the raw models, which would double the dropdown
    }
  ];
in
{
  selfhost.services.chat = {
    displayName = "Chat";
    meta.homepage = "https://github.com/open-webui/open-webui";
    meta.description = "Assistant chat UI";
    meta.category = "productivity";
    port = 3210;
    subdomain = "ai";
    integrations.homepage.icon = "open-webui.svg"; # the default follows the service name, not the app
    access.allowedGroups = [ cfg.groups.admin ];

    # Not forwardAuth: tinyauth classifies by user-agent alone, so it answers this SPA's XHR
    # with a 302 it cannot follow and the app reload-loops.
    access.model = "oidc";
    access.oidc = {
      # Both routes decorate one handler, and `url_for` resolves to whichever registered first.
      callbackURLs = [
        "${serviceCfg.publicUrl}/oauth/oidc/callback"
        "${serviceCfg.publicUrl}/oauth/oidc/login/callback"
      ];
      systemd.dependentServices = [ "open-webui" ];
    };
    healthcheck.path = "/health";
    extraConfig.landingPage.enable = true;
  };

  selfhost.runtimeTemplates."open-webui-env" = {
    content = ''
      OPENAI_API_KEYS=${lib.concatMapStringsSep ";" (b: b.key) backends}
      OAUTH_CLIENT_ID=${cfg.oidcPlaceholder.chat.id}
      OAUTH_CLIENT_SECRET=${cfg.oidcPlaceholder.chat.secret}
    '';
    restartUnits = [ "open-webui.service" ];
  };

  services.open-webui = {
    enable = true;
    inherit (serviceCfg) port;
    environmentFile = cfg.runtimeTemplates."open-webui-env".path;

    environment = {
      SCARF_NO_ANALYTICS = "True";
      DO_NOT_TRACK = "True";
      ANONYMIZED_TELEMETRY = "False";

      WEBUI_URL = serviceCfg.publicUrl;

      ENABLE_PERSISTENT_CONFIG = "False"; # env otherwise only seeds the config DB, which then wins

      OPENID_PROVIDER_URL = "${oidcCfg.provider.issuerUrl}/.well-known/openid-configuration";
      OAUTH_PROVIDER_NAME = oidcCfg.provider.displayName;
      OAUTH_SCOPES = "openid email profile";
      ENABLE_OAUTH_SIGNUP = "True";
      OAUTH_MERGE_ACCOUNTS_BY_EMAIL = "True"; # safe only because the provider verifies addresses

      ENABLE_OLLAMA_API = "False"; # otherwise it probes for an Ollama that is not on this host
      OPENAI_API_BASE_URLS = lib.concatMapStringsSep ";" (b: b.url) backends;
      OPENAI_API_CONFIGS = builtins.toJSON (lib.listToAttrs (map (b: lib.nameValuePair b.url { inherit (b) model_ids; }) backends));
      DEFAULT_MODELS = hermesModel;

      # Each is a whole extra generation per message, queued in front of the answer.
      ENABLE_TITLE_GENERATION = "False";
      ENABLE_TAGS_GENERATION = "False";
      ENABLE_FOLLOW_UP_GENERATION = "False";
      ENABLE_EVALUATION_ARENA_MODELS = "False";
      ENABLE_COMMUNITY_SHARING = "False";
      ENABLE_MESSAGE_RATING = "False";       # single user, so the feedback buttons only add chrome
      ENABLE_VERSION_UPDATE_CHECK = "False"; # nix owns the version
      ENABLE_NOTES = "False";                # the vault is the one place for notes, read via hermes

      # Both default to a pyodide engine, which is 509 MB of the frontend build for a phone to fetch.
      ENABLE_CODE_EXECUTION = "False";
      ENABLE_CODE_INTERPRETER = "False";

      RAG_EMBEDDING_ENGINE = "ollama"; # otherwise it embeds in-process on this host's CPU
      RAG_OLLAMA_BASE_URL = "http://${ai.endpoint.host}:${toString ai.endpoint.port}";
      RAG_EMBEDDING_MODEL = ai.embeddingModel;
    };
  };

  systemd.services.open-webui.serviceConfig = {
    Restart = "on-failure";
    RestartSec = "10s";
    RestartMaxDelaySec = "5min";
    RestartSteps = 5;
  };
}
