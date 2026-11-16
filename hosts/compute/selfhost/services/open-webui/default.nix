{ config, lib, ... }:
let
  cfg = config.selfhost;
  inherit (config.fleet) ai;

  hermesModel = "hermes-agent";
  aiUrl = "http://${ai.endpoint.host}:${toString ai.endpoint.port}/v1";
  aiKey = "none"; # dummy; llama-swap needs no auth

  # URLs, keys and configs are paired by position, so all three come from this one.
  backends = [
    {
      url = aiUrl;
      key = aiKey;
      model_ids = [ ai.model ai.uncensoredModel ];
    }
    {
      url = "http://${ai.assistantEndpoint.host}:${toString ai.assistantEndpoint.port}/v1";
      key = cfg.runtimePlaceholder."hermes-api-server-key";
      model_ids = [ hermesModel ]; # hermes re-exports the raw models, which would double the dropdown
    }
  ];
in
{
  selfhost.apps.open-webui.enable = true;

  # This deployment only: who may reach it.
  selfhost.services.open-webui = {
    displayName = "Chat";
    access.allowedGroups = [ cfg.groups.admin ];
    extraConfig.landingPage.enable = true;
  };

  selfhost.runtimeTemplates."open-webui-backends.env" = {
    content = ''
      OPENAI_API_BASE_URLS=${lib.concatMapStringsSep ";" (b: b.url) backends}
      OPENAI_API_KEYS=${lib.concatMapStringsSep ";" (b: b.key) backends}
      OPENAI_API_CONFIGS=${builtins.toJSON (lib.listToAttrs (map (b: lib.nameValuePair b.url { inherit (b) model_ids; }) backends))}
    '';
    restartUnits = [ "open-webui.service" ];
  };

  services.open-webui = {
    # Free for this; systemd concatenates it with the app's OIDC environment file.
    environmentFile = cfg.runtimeTemplates."open-webui-backends.env".path;

    environment = {
      ENABLE_OLLAMA_API = "False"; # otherwise it probes for an Ollama that is not on this host
      DEFAULT_MODELS = hermesModel;

      # Each is a whole extra generation per message, queued in front of the answer.
      ENABLE_TITLE_GENERATION = "False";
      ENABLE_TAGS_GENERATION = "False";
      ENABLE_FOLLOW_UP_GENERATION = "False";
      ENABLE_EVALUATION_ARENA_MODELS = "False";
      ENABLE_COMMUNITY_SHARING = "False";
      ENABLE_MESSAGE_RATING = "False";       # single user, so the feedback buttons only add chrome
      ENABLE_NOTES = "False";                # the vault is the one place for notes, read via hermes

      # Both default to a pyodide engine, which is 509 MB of the frontend build for a phone to fetch.
      ENABLE_CODE_EXECUTION = "False";
      ENABLE_CODE_INTERPRETER = "False";

      ENABLE_IMAGE_PROMPT_GENERATION = "False"; # a whole extra LLM turn before the diffusion one

      ENABLE_IMAGE_GENERATION = "True";
      IMAGE_GENERATION_ENGINE = "openai";
      IMAGES_OPENAI_API_BASE_URL = aiUrl;
      IMAGES_OPENAI_API_KEY = aiKey;
      IMAGE_GENERATION_MODEL = ai.imageModel;
      IMAGE_SIZE = "1024x1024";

      # No size: the edit keeps the uploaded image's aspect ratio.
      ENABLE_IMAGE_EDIT = "True";
      IMAGE_EDIT_ENGINE = "openai";
      IMAGES_EDIT_OPENAI_API_BASE_URL = aiUrl;
      IMAGES_EDIT_OPENAI_API_KEY = aiKey;
      IMAGE_EDIT_MODEL = ai.imageModel;

      RAG_EMBEDDING_ENGINE = "openai"; # otherwise it embeds in-process on this host's CPU
      RAG_OPENAI_API_BASE_URL = aiUrl;
      RAG_OPENAI_API_KEY = aiKey;
      RAG_EMBEDDING_MODEL = ai.embeddingModel;
      # EmbeddingGemma's task prefixes, applied by Gufo from the checkpoint's named prompts.
      RAG_EMBEDDING_PREFIX_FIELD_NAME = "prompt_name";
      RAG_EMBEDDING_QUERY_PREFIX = "query";
      RAG_EMBEDDING_CONTENT_PREFIX = "document";
    };
  };
}
