{ config, lib, pkgs, ... }:
let
  cfg = config.custom.ai.agents.recipeToCooklang;

  recipeFile = ./recipe.yaml;

  trafilaturaPy = pkgs.python3.withPackages (ps: [ ps.trafilatura ]);

  goose = lib.getExe pkgs.goose-cli;

  worker = pkgs.writeShellApplication {
    name = "agent-recipe-to-cooklang";
    # Available to the .agent/* scripts and to Goose's developer-extension subshells.
    runtimeInputs = with pkgs; [ cook-cli yt-dlp gallery-dl trafilaturaPy curl jq coreutils findutils ];
    text = ''
      set -euo pipefail

      INBOX=${lib.escapeShellArg cfg.inboxDir}
      RECIPES=${lib.escapeShellArg cfg.outputDir}
      PROCESSING="$INBOX/../processing"
      FAILED="$INBOX/../failed"

      # Touch parent dirs to trigger SMB automount; bail quietly if unavailable.
      if ! ls "$INBOX" >/dev/null 2>&1 || ! ls "$RECIPES" >/dev/null 2>&1; then
        echo "SMB mounts not ready, skipping run"
        exit 0
      fi

      # Pre-flight: required scripts must exist and be executable. Otherwise
      # we'd burn through the whole queue producing identical failures.
      for script in fetch run; do
        if [ ! -x "$RECIPES/.agent/$script" ]; then
          echo "Missing or non-executable: $RECIPES/.agent/$script — skipping run"
          exit 0
        fi
      done

      mkdir -p "$PROCESSING" "$FAILED"

      shopt -s nullglob
      for job in "$INBOX"/*.txt; do
        name=$(basename "$job" .txt)
        claimed="$PROCESSING/$name.txt"
        staging=$(mktemp -d -t "agent-recipe-XXXXXX")

        # Atomic claim — if mv fails, another worker won or the file vanished.
        mv "$job" "$claimed" 2>/dev/null || { rm -rf "$staging"; continue; }

        url=$(tr -d '[:space:]' < "$claimed")

        # Agent populates staging. Goose's exit code is informational only —
        # the real success signal is .agent/run exiting 0 below.
        ${goose} run \
            --recipe ${recipeFile} \
            --params "url=$url" \
            --params "recipes_dir=$RECIPES" \
            --params "staging_dir=$staging" || true

        # Publish step runs OUTSIDE the LLM loop. This is the only code path
        # that touches files in $RECIPES; the LLM cannot direct it.
        if "$RECIPES/.agent/run" "$staging"; then
          rm -rf "$staging" "$claimed"
        else
          mkdir -p "$FAILED/$name"
          mv "$claimed" "$FAILED/$name/source.txt"
          mv "$staging" "$FAILED/$name/staging"
        fi
      done
    '';
  };
in
{
  options.custom.ai.agents.recipeToCooklang = {
    enable = lib.mkEnableOption "URL → cooklang recipe agent";

    inboxDir = lib.mkOption {
      type = lib.types.str;
      description = "Directory polled for `*.txt` jobs (one URL per file).";
    };

    outputDir = lib.mkOption {
      type = lib.types.str;
      description = "Recipes collection root. Must contain AGENTS.md and .agent/{fetch,run}.";
    };

    model = lib.mkOption {
      type = lib.types.str;
      default = "qwen3:8b";
      description = "Ollama model used by Goose for this agent.";
    };

    runtimeMaxSec = lib.mkOption {
      type = lib.types.str;
      default = "15min";
      description = "Hard cap per service run; prevents runaway agent loops from pinning VRAM.";
    };
  };

  config = lib.mkIf cfg.enable {
    systemd.services.agent-recipe-to-cooklang = {
      description = "Recipe → cooklang agent (queue worker)";
      after = [ "ollama.service" "network-online.target" ];
      wants = [ "network-online.target" ];

      environment = {
        GOOSE_PROVIDER = "ollama";
        GOOSE_MODEL = cfg.model;
        OLLAMA_HOST = "http://127.0.0.1:11434";
        GOOSE_DISABLE_KEYRING = "true";
        OTEL_SDK_DISABLED = "true";
      };

      serviceConfig = {
        Type = "oneshot";
        ExecStart = lib.getExe worker;
        RuntimeMaxSec = cfg.runtimeMaxSec;

        DynamicUser = true;
        SupplementaryGroups = [
          config.custom.homelab.smb.mounts.bphenriques.group
          config.custom.homelab.smb.mounts.media.group
        ];
        RuntimeDirectory = "agent-recipe-to-cooklang";

        Environment = [
          "HOME=%t/agent-recipe-to-cooklang"
          "XDG_CONFIG_HOME=%t/agent-recipe-to-cooklang/config"
          "XDG_DATA_HOME=%t/agent-recipe-to-cooklang/data"
          "XDG_CACHE_HOME=%t/agent-recipe-to-cooklang/cache"
        ];

        ReadWritePaths = [ cfg.inboxDir cfg.outputDir ];
        # Defense-in-depth: the agent has no legitimate reason to write to
        # AGENTS.md or .agent/. RO-bind so the kernel enforces it. Leading
        # `-` makes them optional if the SMB mount is lazily activated.
        BindReadOnlyPaths = [
          "-${cfg.outputDir}/AGENTS.md"
          "-${cfg.outputDir}/.agent"
        ];

        ProtectSystem = "strict";
        ProtectHome = true;
        PrivateTmp = true;
        NoNewPrivileges = true;
        ProtectKernelTunables = true;
        ProtectKernelModules = true;
        ProtectControlGroups = true;
        RestrictSUIDSGID = true;
        RestrictNamespaces = true;
        LockPersonality = true;
        RestrictAddressFamilies = [ "AF_INET" "AF_INET6" "AF_UNIX" ];
      };
    };

    systemd.timers.agent-recipe-to-cooklang = {
      description = "Poll recipe inbox";
      wantedBy = [ "timers.target" ];
      timerConfig = {
        OnUnitInactiveSec = "60s";
        Persistent = false;
      };
    };
  };
}
