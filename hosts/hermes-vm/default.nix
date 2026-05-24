# Hermes VM — guest-side NixOS configuration.
#
# Phase 1 (blue/green migration): minimal VM running hermes-agent, no
# external API server, no Traefik route. SSH-accessible from compute
# (10.20.1.1) for smoke testing. Once verified, phase 2 adds the API
# server (sops in VM), Traefik routing, and decommissions the
# compute-side hermes-agent (blue).
{ config, pkgs, inputs, ... }:
let
  fleet = import ../shared.nix;
  laptopIP = fleet.lan.hosts.laptop;
  # Vault is virtiofs-shared from compute at this mount point — same
  # absolute path as on the host so SOUL.md / AGENTS.md guidance stays
  # source-of-truth without per-host string juggling.
  vaultPath = "/mnt/homelab-bphenriques/notes";
  mcpvault = pkgs.writeShellScript "mcpvault" ''
    exec ${pkgs.nodejs}/bin/npx -y @bitbonsai/mcpvault@0.11.0 "$@"
  '';
in
{
  imports = [
    inputs.microvm.nixosModules.microvm
    inputs.hermes-agent.nixosModules.default
    ./microvm.nix
  ];

  networking.hostName = "hermes-vm";
  system.stateVersion = "25.05";

  # Boot, root, time, locale — minimal headless server defaults.
  time.timeZone = "Europe/Lisbon";
  i18n.defaultLocale = "en_US.UTF-8";
  console.keyMap = "pt-latin1";

  # SSH for interactive access. `microvm -c` and `microvm -l` require
  # the host to evaluate the flake, which our build-on-laptop /
  # deploy-closure-to-compute model rules out. SSH is the practical
  # workaround — VM is reachable only at 10.20.1.10 on the bridge, so
  # this isn't an externally-visible service.
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
    # Persist host keys on the /var/lib/hermes volume so SSH identity
    # survives VM rebuilds. Without this, every rebuild regenerates
    # them and laptop's known_hosts has to be cleared each time.
    hostKeys = [
      { path = "/var/lib/hermes/.ssh-host-keys/ssh_host_ed25519_key"; type = "ed25519"; }
      { path = "/var/lib/hermes/.ssh-host-keys/ssh_host_rsa_key"; type = "rsa"; bits = 4096; }
    ];
  };

  # Let the hermes user read the system journal so debugging
  # hermes-agent doesn't require sudo. systemd-journal is the
  # least-privilege group for this; wheel would also grant sudo.
  users.users.hermes.extraGroups = [ "systemd-journal" ];
  # Public key copied from the laptop user (same key used historically
  # for the `auth` microvm). Pubkeys are public — no secret-management
  # needed.
  users.users.hermes.openssh.authorizedKeys.keys = [
    "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBETAZZTh/Czemis4B6JKqySKLqWn5IUPqIvaJbEIe/3"
  ];

  # Hermes user needs a writable HOME for the npx cache (mcpvault) and
  # state directories. /var/lib/hermes is on its own volume (see
  # microvm.nix) so it persists across VM rebuilds.

  # Hermes Agent service. Same shape as compute's config minus the
  # homelab framework wrapping — VM doesn't run Traefik, secrets, or
  # the dashboard for now. Phase 2 layers those back in.
  services.hermes-agent = {
    enable = true;
    addToSystemPackages = true;
    # "web" → FastAPI/Uvicorn (the API server); "pty" → embedded TUI chat.
    extraDependencyGroups = [ "web" "pty" ];
    # API_SERVER_* (incl. API_SERVER_KEY) come from a template rendered on
    # compute and virtiofs-shared into the VM at /run/host-secrets/env.
    # See hosts/compute/services/hermes-vm-api.nix.
    environmentFiles = [ "/run/host-secrets/env" ];

    mcpServers = {
      fetch.command = "${pkgs.mcp-server-fetch}/bin/mcp-server-fetch";
      time.command = "${pkgs.mcp-server-time}/bin/mcp-server-time";
      vault = {
        command = "${mcpvault}";
        args = [ vaultPath ];
      };
    };

    # SOUL.md copied verbatim from compute's blue config so we're
    # comparing apples to apples during blue/green. Once green wins,
    # we'll dedupe by extracting to a shared file or moving the
    # canonical source here.
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
        default = "gemma4:e4b";
        provider = "custom";
        api_key = "ollama";
        base_url = "http://${laptopIP}:11434/v1";
        context_length = 65536;  # Hermes Agent enforces a 64K minimum.
        max_tokens = 8192;
      };
      auxiliary.compression.context_length = 65536;
      dashboard.theme = "mono";
      display.theme = "mono";
      honcho.mode = "local";
      platform_toolsets = {
        cli        = [ "vault" "fetch" "time" ];
        api_server = [ "vault" "fetch" "time" ];
      };
    };
  };

  systemd.services.hermes-agent.serviceConfig = {
    TimeoutStopSec = "210s";
    # The upstream module concatenates `services.hermes-agent.environmentFiles`
    # into $HERMES_HOME/.env via an activation script — but the volume
    # mount at /var/lib/hermes shadows that write, so it never appears.
    # Inject the host-shared env file as a real systemd EnvironmentFile=
    # so API_SERVER_* reach the process directly. Python-dotenv's
    # default doesn't override existing env vars, so the systemd path
    # wins regardless of whether .env is also present.
    EnvironmentFile = [ "/run/host-secrets/env" ];
    # Same volume-shadow issue affects config.yaml. The host renders it
    # into the host-secrets share (see hermes-vm-api.nix); we copy it
    # into $HERMES_HOME on every service start, post-mount.
    # `+` prefix lets the command run as root (needed to chown to the
    # hermes user; the service itself runs as hermes).
    ExecStartPre = [
      "+${pkgs.coreutils}/bin/install -D -m 0640 -o hermes -g hermes /run/host-secrets/config.yaml /var/lib/hermes/.hermes/config.yaml"
    ];
  };
}
