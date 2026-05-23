#
# Memory audit (recurring manual task — not automated):
#   Hermes uses Honcho to extract long-term facts about you from
#   conversations. They land in two files under HERMES_HOME:
#     /var/lib/hermes/.hermes/memories/MEMORY.md   (events, preferences, facts)
#     /var/lib/hermes/.hermes/memories/USER.md     (user-modeling summary)
#
#   Audit cadence: every few weeks, or after a sensitive conversation.
#     sudo cat /var/lib/hermes/.hermes/memories/MEMORY.md
#     sudo cat /var/lib/hermes/.hermes/memories/USER.md
#
#   To remove or amend: edit those files in place as the `hermes` user
#   (Hermes treats them as live state, no migration needed), or ask the
#   agent in chat: "forget that I X" / "stop tracking Y".
#
#   For tighter boundaries on what gets extracted: edit the "Memory
#   boundaries" section in documents."SOUL.md" below — that's the
#   single instruction the agent reads on every turn.
#
{ inputs, config, lib, pkgs, ... }:
let
  laptopIP = config.custom.fleet.lan.hosts.laptop;
  serviceCfg = config.custom.homelab.services.hermes-api;
  dashboardCfg = config.custom.homelab.services.hermes;
  hermesPackage = inputs.hermes-agent.packages.${pkgs.system}.default;
  homelabMounts = config.custom.homelab.smb.mounts;
  vaultPath = config.custom.homelab.paths.users.bphenriques.notes;
  # mcpvault (https://github.com/bitbonsai/mcpvault) — Obsidian-aware
  # MCP server. Vault path bound server-side, so tool args are
  # vault-relative (e.g. "finance", "inbox/note.md"); model can't
  # produce path-traversal failures. Pulled from npm on first run via
  # npx; cached under hermes user's HOME thereafter. Experiment phase:
  # promote to a sealed buildNpmPackage derivation if we keep it.
  mcpvault = pkgs.writeShellScript "mcpvault" ''
    exec ${pkgs.nodejs}/bin/npx -y @bitbonsai/mcpvault@0.11.0 "$@"
  '';
in
{
  imports = [ inputs.hermes-agent.nixosModules.default ];

  # API server bound to loopback, exposed via Traefik at hermes-api.{domain}.
  # Clients resolve the FQDN locally (laptop /etc/hosts → compute IP) so the
  # connection stays on LAN/WG while preserving HTTPS via the wildcard cert.
  # Grant the hermes user read access to the Obsidian vault on the NAS.
  users.users.hermes.extraGroups = [ homelabMounts.bphenriques.group ];

  custom.homelab.services.hermes-api = {
    displayName = "Hermes API";
    metadata.description = "Personal assistant — OpenAI-compatible API";
    metadata.version = config.services.hermes-agent.package.version;
    metadata.homepage = "https://hermes-agent.nousresearch.com/";
    metadata.category = "Infrastructure";
    port = 8642;
    subdomain = "hermes-api";
    integrations.monitoring.enable = false;
    integrations.homepage.enable = false;
    storage.smb = [ "bphenriques" ];
    storage.systemdServices = [ "hermes-agent" ];
    secrets = {
      files.api-token = { rotatable = true; bytes = 32; };
      templates."env".content = ''
        API_SERVER_ENABLED=true
        API_SERVER_PORT=8642
        API_SERVER_HOST=127.0.0.1
        API_SERVER_KEY=${serviceCfg.secrets.placeholder.api-token}
      '';
      systemd.dependentServices = [ "hermes-agent" ];
    };
  };

  systemd.services.hermes-agent = {
    # Restart the gateway when any persistent state input changes —
    # Hermes reads .env, config.yaml, mcp_servers, and documents once
    # at startup, so plain file regeneration isn't enough.
    restartTriggers = [
      serviceCfg.secrets.templates."env".content
      (builtins.toJSON config.services.hermes-agent.settings)
      (builtins.toJSON config.services.hermes-agent.mcpServers)
      (builtins.toJSON config.services.hermes-agent.documents)
    ];
    # Hermes's drain timeout is 180s; default TimeoutStopSec=90s causes
    # SIGKILL mid-drain on every restart (exit code 1 + state mid-write).
    serviceConfig.TimeoutStopSec = "210s";
  };

  # Browser UI for admin (sessions, cron, skills, memory, logs) + embedded
  # TUI chat for power use. The day-to-day friendly chat surface is NextChat
  # at chat.{domain}; this dashboard is for inspecting/tuning the agent.
  custom.homelab.services.hermes = {
    displayName = "Hermes";
    metadata.description = "Personal assistant — admin + TUI chat";
    metadata.version = config.services.hermes-agent.package.version;
    metadata.homepage = "https://hermes-agent.nousresearch.com/";
    metadata.category = "Productivity";
    port = 9119;
    subdomain = "hermes";
    integrations.monitoring.enable = false;
    integrations.homepage.enable = true;
  };

  systemd.services.hermes-dashboard = {
    description = "Hermes dashboard (web UI)";
    wantedBy = [ "multi-user.target" ];
    after = [ "hermes-agent.service" ];
    serviceConfig = {
      User = "hermes";
      Group = "hermes";
      Environment = [
        "HERMES_HOME=/var/lib/hermes/.hermes"
        "HOME=/var/lib/hermes"
      ];
      # --insecure tells the dashboard to skip its built-in Host-header
      # validation (DNS rebinding defense). Required when behind a reverse
      # proxy that rewrites Host. Our trust chain stays intact:
      #   - Traefik fronts requests via bond0/wg0 only (firewall on 80/443).
      #   - Port 9119 is not in the homelab's allowedTCPPorts; only Traefik
      #     on the same host (compute) can reach it.
      #   - Dashboard's own session-token auth still applies.
      ExecStart = "${hermesPackage}/bin/hermes dashboard --tui "
        + "--host 0.0.0.0 --port ${toString dashboardCfg.port} "
        + "--no-open --insecure";
      Restart = "on-failure";
      RestartSec = "10s";
    };
  };

  services.hermes-agent = {
    enable = true;
    addToSystemPackages = true;  # `hermes` CLI on system PATH
    # `web` ships FastAPI/Uvicorn for the dashboard's HTTP server;
    # `pty` enables the embedded TUI chat pane inside the browser.
    extraDependencyGroups = [ "web" "pty" ];
    environmentFiles = [ serviceCfg.secrets.templates."env".path ];

    # MCP tools available to the agent at runtime.
    # Stick to nixpkgs servers — zero maintenance, no custom code.
    #
    # NOTE: explicit `/bin/<name>` instead of lib.getExe. nixpkgs's
    # mcp-server-time has meta.mainProgram = "mcp-server-git" (upstream
    # bug), so lib.getExe yields a non-existent binary path. Worth a
    # PR upstream; for now, explicit paths are safer for all of them.
    mcpServers = {
      fetch.command = "${pkgs.mcp-server-fetch}/bin/mcp-server-fetch";
      time.command = "${pkgs.mcp-server-time}/bin/mcp-server-time";
      # Obsidian-aware vault server (see mcpvault let-binding above for
      # rationale + rollback notes). Tool surface: read_note,
      # write_note, list_directory, search_notes (content + filename
      # with BM25), manage_tags, update_frontmatter, et al. — paths
      # are vault-relative.
      vault = {
        command = "${mcpvault}";
        args = [ vaultPath ];
      };
    };

    # SOUL.md persona — installed to $MESSAGING_CWD/SOUL.md so the agent
    # reads it as the project-level persona. Edit here, rebuild to apply.
    documents."SOUL.md" = ''
      # Hermes Persona

      You are the user's personal assistant on their home infrastructure.

      ## User context
      - Portugal, Europe/Lisbon, EUR. Notes may be in Portuguese or English.
      - Software engineer; comfortable with NixOS, CLI, self-hosted services.
      - Privacy-first: prefer local/self-hosted over cloud. No data leaves
        the home network without reason.

      ## Response style
      - Terse. Lead with the answer. One sentence often suffices.
      - Markdown structure when it helps. Dates `YYYY-MM-DD`, 24h time, EUR.
      - Voice inputs (Pebble watch) may have dictation errors — infer intent
        liberally; only ask for clarification when ambiguous. Keep replies
        short (push-notification delivery).

      ## Vault
      - Obsidian vault is reachable via the `vault` MCP server using
        vault-relative paths. Read the root `AGENTS.md` first — it points
        to `README.md` for layout and adds agent-specific guidance
        (skip-list, language hints, table conventions).
      - Always skip `.trash/` and dotfile-prefixed folders (`.obsidian/`,
        `.git/`, etc.) — sync/system state, not notes.
      - When `search_notes` underperforms (tables, cross-language hits),
        fall back to listing and reading the relevant subfolder directly.
      - Use Obsidian Markdown when writing (wikilinks, callouts,
        frontmatter, tags). Respect existing folder/naming conventions.

      ## Memory
      - Sole-user local deployment — default to remembering preferences,
        tooling choices, projects, recurring patterns.
      - Honour explicit "forget X" / "don't remember Y" as durable.
    '';

    settings = {
      model = {
        # TODO: link to a fleet-wide constant so Ollama's loadModels (laptop)
        # and Hermes's default model (compute) can't drift. e.g. via
        # custom.fleet.ai.primaryModel in lib/fleet.nix or similar.
        #
        # gemma4:e4b — Gemma 4 MatFormer "edge" variant, ~4B active per token,
        # 9.6 GB Q4 (fits mostly in 8 GB VRAM with small RAM spill). Native
        # function calling, 128K context. Right-sized for notes / calendar /
        # fetch / summarisation workloads. Step up to gemma4:26b if deeper
        # reasoning or multi-doc synthesis becomes a routine ask.
        # Predecessors tried: qwen2.5:7b (called tools but ignored SOUL path
        # binding), hermes3:8b, llama3.1:8b (both 0 tool calls).
        default = "gemma4:e4b";
        provider = "custom";  # bypasses provider abstraction; uses base_url directly
        api_key = "ollama";  # dummy; Ollama doesn't require auth
        base_url = "http://${laptopIP}:11434/v1";
        # Hermes Agent enforces a hard 64K minimum (rejects requests below
        # that with "context window … below the minimum 64,000"). Cannot
        # be lowered, even for short single-turn watch workloads.
        context_length = 65536;
        max_tokens = 8192;
      };
      auxiliary.compression.context_length = 65536;

      # Minimal aesthetic. Options:
      #   dashboard.theme: default, midnight, ember, mono, cyberpunk, rose
      #   display.theme:   default, ares, mono, slate   (CLI/embedded TUI)
      dashboard.theme = "mono";
      display.theme = "mono";

      # Memory: keep extraction fully local (no Honcho cloud round-trip).
      # Explicit even if default — guards against upstream default flips.
      honcho.mode = "local";

      # Pin MCP toolsets per platform. Hermes auto-includes all enabled MCP
      # servers when a platform's list omits them, but listing them
      # explicitly converts the rule from "include all" to "allowlist these",
      # making it visible at config-review time exactly what each surface
      # advertises to the model.
      #   cli         → `hermes chat` (terminal)
      #   api_server  → NextChat / external OpenAI-compatible callers
      platform_toolsets = {
        cli        = [ "vault" "fetch" "time" ];
        api_server = [ "vault" "fetch" "time" ];
      };
    };
  };
}
