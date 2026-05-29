# Hermes VM — guest-side NixOS configuration.
#
# Runs hermes-agent (Nous personal assistant) as a service user with no
# shell or sudo. Human admin access is via the `bphenriques` account,
# reachable over SSH (ProxyJump through compute) on the bridge IP.
{ config, pkgs, inputs, private, ... }:
let
  fleet = import ../shared.nix;
  laptopIP = fleet.lan.hosts.laptop;
  # Local clone of the obsidian vault from gitea (see vault-sync below).
  # The hermes user can read but not mutate (clone is owned root:hermes,
  # 0750 — git operations run as root via vault-sync.service).
  vaultPath = "/var/lib/hermes/vault";
  # Domain comes from compute's private settings — hermes-vm is hosted on
  # compute and shares the same homelab, so the gitea URL is identical.
  homelabDomain = inputs.dotfiles-private.hosts.compute.settings.domain;
  vaultRepoUrl = "https://hermes-agent@git.${homelabDomain}/bphenriques/notes.git";
  vaultSync = pkgs.writeShellApplication {
    name = "vault-sync";
    runtimeInputs = [ pkgs.git ];
    text = ''
      set -euo pipefail
      export GIT_ASKPASS=${pkgs.writeShellScript "gitea-askpass" ''
        cat ${config.sops.secrets."hermes-agent/gitea-token".path}
      ''}
      export GIT_TERMINAL_PROMPT=0
      if [ -d ${vaultPath}/.git ]; then
        git -C ${vaultPath} fetch --quiet --depth 1 origin
        git -C ${vaultPath} reset --hard --quiet FETCH_HEAD
      else
        git clone --quiet --depth 1 ${vaultRepoUrl} ${vaultPath}
      fi
      # Re-apply on every sync — git can create new files/dirs with
      # default perms; we want them readable by the hermes group only.
      chown -R root:hermes ${vaultPath}
      chmod -R u=rwX,g=rX,o= ${vaultPath}
    '';
  };
in
{
  imports = [
    inputs.microvm.nixosModules.microvm
    inputs.hermes-agent.nixosModules.default
    ./microvm.nix
  ];

  networking.hostName = "hermes-vm";
  system.stateVersion = "26.05";

  # Boot, root, time, locale — minimal headless server defaults.
  time.timeZone = "Europe/Lisbon";
  i18n.defaultLocale = "en_US.UTF-8";
  console.keyMap = "pt-latin1";

  # SSH is for the human admin (bphenriques), not for the agent. The VM
  # is reachable only on the compute-microvm bridge, so SSH is gated
  # behind ProxyJump-through-compute and the fleet-wide hardening
  # profile (key-only, no forwarding) still applies.
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
    ];
  };

  # Human admin account: only bphenriques can perform rootful operations
  # on the VM for inspection. The hermes service user (created by the
  # upstream module) stays minimal — no shell, no sudo, no SSH.
  users.users.bphenriques = {
    isNormalUser = true;
    extraGroups = [ "wheel" "systemd-journal" ];
    openssh.authorizedKeys.keys = fleet.authorizedSSHKeys;
  };
  # Passwordless sudo is acceptable here: SSH is key-only and the VM is
  # unreachable from outside compute's bridge.
  security.sudo.wheelNeedsPassword = false;

  # Hermes Agent service. Traefik routing happens on compute via
  # hosts/compute/services/hermes-api.nix — the VM only exposes the
  # raw OpenAI-compatible API on 0.0.0.0:8642.
  services.hermes-agent = {
    enable = true;
    addToSystemPackages = true;
    # "web" → FastAPI/Uvicorn (the API server); "pty" → embedded TUI chat.
    extraDependencyGroups = [ "web" "pty" ];
    # API_SERVER_* are rendered from sops-decrypted secrets via the
    # `env` template below. The path is set as systemd EnvironmentFile=.
    # See hosts/hermes-vm/README.md for the bootstrap.
    environmentFiles = [ config.sops.templates."hermes-env".path ];

    mcpServers = {
      fetch.command = "${pkgs.mcp-server-fetch}/bin/mcp-server-fetch";
      time.command = "${pkgs.mcp-server-time}/bin/mcp-server-time";
      vault = {
        command = "${pkgs.nodejs}/bin/npx";
        args = [ "-y" "@bitbonsai/mcpvault@0.11.0" vaultPath ];
      };
    };

    documents."SOUL.md" = builtins.readFile ./SOUL.md;

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

  # sops-nix's activation runs in initrd, but /var/lib/hermes (where the
  # SSH host key sops uses as its age identity lives) isn't mounted yet
  # there. Re-run activation in stage 2 once local-fs is up; activation
  # is idempotent.
  systemd.services.sops-late-activate = {
    description = "Re-run NixOS activation in stage 2 so sops can decrypt off the SSH host key on /var/lib/hermes";
    wantedBy = [ "multi-user.target" ];
    before = [ "vault-sync.service" "hermes-agent.service" ];
    unitConfig.ConditionPathExists = "/var/lib/hermes/.ssh-host-keys/ssh_host_ed25519_key";
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "/run/current-system/activate";
    };
  };

  # Pull the vault from gitea at boot (before hermes-agent) and refresh
  # every 5 min. Runs as root so the clone can be owned root:hermes
  # 0750 — hermes reads only. There is no push path; local edits get
  # blown away on the next sync.
  systemd.services.vault-sync = {
    description = "Sync hermes vault from gitea";
    after = [ "network-online.target" "sops-late-activate.service" ];
    wants = [ "network-online.target" ];
    requires = [ "sops-late-activate.service" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${vaultSync}/bin/vault-sync";
    };
  };

  systemd.timers.vault-sync = {
    description = "Periodic vault sync from gitea";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "30s";
      OnUnitActiveSec = "5min";
    };
  };

  systemd.services.hermes-agent = {
    # Make sure the vault is on disk before the agent starts.
    requires = [ "vault-sync.service" ];
    after = [ "vault-sync.service" ];
    serviceConfig = {
      TimeoutStopSec = "210s";
      # The upstream module concatenates `environmentFiles` into
      # $HERMES_HOME/.env via an activation script — but the volume
      # mount at /var/lib/hermes shadows that write, so it never appears.
      # Inject the env file as a real systemd EnvironmentFile= so the
      # API_SERVER_* values reach the process directly. Python-dotenv's
      # default doesn't override existing env vars, so the systemd path
      # wins regardless of what hermes-agent does with .env itself.
      EnvironmentFile = [ config.sops.templates."hermes-env".path ];
      # Same volume-shadow issue affects config.yaml. The settings are
      # rendered to a path inside the VM's own /etc (in-VM closure, no
      # host coupling); ExecStartPre installs them into $HERMES_HOME on
      # every service start, post-mount. `+` prefix runs as root so we
      # can chown to hermes.
      ExecStartPre = [
        "+${pkgs.coreutils}/bin/install -D -m 0640 -o hermes -g hermes /etc/hermes-agent/config.yaml /var/lib/hermes/.hermes/config.yaml"
      ];
    };
  };

  # sops-nix: this VM decrypts its own secrets using its ed25519 SSH host
  # key as the age identity (sops-nix converts ssh-ed25519 keys to age
  # automatically). The host key lives on /var/lib/hermes and is also
  # used as the SSH identity, so there's a single key to manage.
  # The encrypted file lives in dotfiles-private/hosts/hermes-vm/secrets.yaml.
  sops = {
    defaultSopsFile = private.sopsSecretsFile;
    age.sshKeyPaths = [ "/var/lib/hermes/.ssh-host-keys/ssh_host_ed25519_key" ];
    secrets."hermes-agent/api-server-key" = { };
    # Personal access token for vault-sync's git clone. Generated once on
    # compute (see hermes-vm/README.md); stored as plaintext in the VM's
    # sops file. Read-only repo scope.
    secrets."hermes-agent/gitea-token" = {
      owner = "hermes";
      mode = "0400";
    };
    templates."hermes-env" = {
      content = ''
        API_SERVER_ENABLED=true
        API_SERVER_PORT=8642
        API_SERVER_HOST=0.0.0.0
        API_SERVER_KEY=${config.sops.placeholder."hermes-agent/api-server-key"}
      '';
      owner = "hermes";
      mode = "0400";
    };
  };

  # Render config.yaml from the VM's own evaluated settings, baked into
  # the closure as a non-secret static file. ExecStartPre (above)
  # installs it into $HERMES_HOME after the volume mounts.
  environment.etc."hermes-agent/config.yaml".source =
    pkgs.writeText "hermes-config.yaml"
      (builtins.toJSON config.services.hermes-agent.settings);
}
