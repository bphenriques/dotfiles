{ config, lib, ... }:
let
  cfg = config.selfhost;
  inherit (config.fleet) ai;
  agentVmIp = config.fleet.microvms.compute.agent-vm;
  agentVm = import ../../../../guests/agent-vm/settings.nix;

  hermesModel = "hermes-agent";
  comfyUrl = "http://${ai.imageEndpoint.host}:${toString ai.imageEndpoint.port}";

  # kyuz0's validated workflow, with the unet swapped bf16 -> fp8mixed to halve what it loads.
  # Compacted because an EnvironmentFile line cannot hold newlines.
  imageEditWorkflow = builtins.toJSON (builtins.fromJSON (builtins.readFile ./qwen-image-edit.json));

  # Only these three are safe: `image_edits` never populates steps, and the edit form has no
  # negative_prompt field, so mapping either writes a null into the graph. Steps stays at the
  # LoRA's 4, and output size follows the uploaded image rather than IMAGE_EDIT_SIZE.
  imageEditNodes = builtins.toJSON [
    { type = "prompt"; node_ids = [ "68" ]; key = "prompt"; }
    { type = "image"; node_ids = [ "41" ]; key = "image"; }
    { type = "seed"; node_ids = [ "65" ]; key = "seed"; }
  ];

  # Same treatment. Upstream's JSON loads the *Edit* LoRA into a text-to-image graph, which reads
  # as a copy-paste slip, so this names the matching Qwen-Image one that menu entry 3 fetches.
  imageWorkflow = builtins.toJSON (builtins.fromJSON (builtins.readFile ./qwen-image.json));

  # negative_prompt is left out for the reason it is above: the Images playground never sends one,
  # and a mapped node would write a null. The rest are always populated for generation.
  imageNodes = builtins.toJSON [
    { type = "prompt"; node_ids = [ "6" ]; key = "text"; }
    { type = "width"; node_ids = [ "58" ]; key = "width"; }
    { type = "height"; node_ids = [ "58" ]; key = "height"; }
    { type = "n"; node_ids = [ "58" ]; key = "batch_size"; }
    { type = "steps"; node_ids = [ "3" ]; key = "steps"; }
    { type = "seed"; node_ids = [ "3" ]; key = "seed"; }
  ];

  # URLs, keys and configs are paired by position, so all three come from this one.
  backends = [
    {
      url = "http://${ai.endpoint.host}:${toString ai.endpoint.port}/v1";
      key = "ollama"; # dummy; Ollama needs no auth
      model_ids = [ ai.model ai.codingModel ];
    }
    {
      url = "http://${agentVmIp}:${toString agentVm.apiPort}/v1";
      key = cfg.runtimePlaceholder."hermes-api-server-key";
      model_ids = [ hermesModel ]; # hermes re-exports the raw models, which would double the dropdown
    }
  ];
in
{
  selfhost.apps.open-webui.enable = true;

  # The app registers the entry, the OIDC client, the login-form lockout and the telemetry opt-outs.
  # What stays is this deployment: who may reach it, and which models and image pipelines it talks to.
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
      IMAGES_EDIT_COMFYUI_WORKFLOW=${imageEditWorkflow}
      IMAGES_EDIT_COMFYUI_WORKFLOW_NODES=${imageEditNodes}
      COMFYUI_WORKFLOW=${imageWorkflow}
      COMFYUI_WORKFLOW_NODES=${imageNodes}
    '';
    restartUnits = [ "open-webui.service" ];
  };

  services.open-webui = {
    # The app leaves `environmentFile` free for exactly this; systemd concatenates it with the one
    # carrying the OIDC credentials.
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

      ENABLE_IMAGE_EDIT = "True";
      IMAGE_EDIT_ENGINE = "comfyui";
      IMAGES_EDIT_COMFYUI_BASE_URL = comfyUrl;
      ENABLE_IMAGE_PROMPT_GENERATION = "False"; # a whole extra LLM turn before the diffusion one

      ENABLE_IMAGE_GENERATION = "True";
      IMAGE_GENERATION_ENGINE = "comfyui";
      COMFYUI_BASE_URL = comfyUrl;
      # Both reach the graph, unlike on the edit side: Qwen-Image is 1328x1328 native, and the
      # Lightning LoRA wants 4 steps rather than the 50 this would otherwise default to.
      IMAGE_SIZE = "1328x1328";
      IMAGE_STEPS = "4";

      RAG_EMBEDDING_ENGINE = "ollama"; # otherwise it embeds in-process on this host's CPU
      RAG_OLLAMA_BASE_URL = "http://${ai.endpoint.host}:${toString ai.endpoint.port}";
      RAG_EMBEDDING_MODEL = ai.embeddingModel;
    };
  };
}
