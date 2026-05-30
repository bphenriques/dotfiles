# personal-agent — guest-side NixOS configuration for the assistant microvm.
#
# Runs the assistant runtime (currently hermes-agent) as a service user with
# no shell or sudo. Human admin access is via the `bphenriques` account,
# reachable over SSH (ProxyJump through compute) on the bridge IP.
{ config, pkgs, inputs, self, private, ... }:
let
  fleet = import ../shared.nix;
  laptopIP = fleet.lan.hosts.laptop;
  # Local clone of the obsidian vault from gitea (see vault-sync below).
  # The hermes user can read but not mutate (clone is owned root:hermes,
  # 0750 — git operations run as root via vault-sync.service).
  vaultPath = "/var/lib/hermes/vault";
  # Domain comes from compute's private settings — personal-agent is hosted on
  # compute and shares the same homelab, so the gitea URL is identical.
  homelabDomain = inputs.dotfiles-private.hosts.compute.settings.domain;
  vaultRepoUrl = "ssh://gitea@git.${homelabDomain}:2222/bphenriques/notes.git";
  # ssh wrapper for vault-sync: pin the identity to the VM's host key
  # (also the sops age key — single trust anchor), put known_hosts on
  # the persistent volume, accept the gitea host key on first contact.
  vaultSshCommand = pkgs.writeShellScript "vault-sync-ssh" ''
    exec ${pkgs.openssh}/bin/ssh \
      -i /var/lib/hermes/.ssh-host-keys/ssh_host_ed25519_key \
      -o UserKnownHostsFile=/var/lib/hermes/.ssh-host-keys/known_hosts \
      -o StrictHostKeyChecking=accept-new \
      -o BatchMode=yes \
      "$@"
  '';
  vaultSync = pkgs.writeShellApplication {
    name = "vault-sync";
    runtimeInputs = [ pkgs.git ];
    text = ''
      set -euo pipefail
      export GIT_SSH_COMMAND=${vaultSshCommand}
      if [ -d ${vaultPath}/.git ]; then
        # Re-point on every sync so a Nix-level URL change (transport swap,
        # repo rename, …) flows into .git/config without manual surgery.
        git -C ${vaultPath} remote set-url origin ${vaultRepoUrl}
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
    # mkMicrovmGuest deliberately doesn't load all self.nixosModules
    # (would pull in homelab too); pick the two we need explicitly.
    self.nixosModules.ai             # pulls in inputs.hermes-agent transitively
    self.nixosModules.fleet-shared   # `custom.fleet.*` options, used by `custom.ai.model`'s default
    ./microvm.nix
  ];

  networking.hostName = "personal-agent";
  system.stateVersion = "26.05";

  # Fleet config — populates the `custom.fleet.*` options the AI module
  # reads (specifically `custom.fleet.ai.model` as the default for
  # `custom.ai.model`).
  custom.fleet = import ../shared.nix;

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

  # Hermes runtime via the AI module. Traefik routing happens on compute
  # (`hosts/compute/services/hermes-api.nix`); the VM only exposes the raw
  # OpenAI-compatible API. Model defaults to fleet config (gemma4:e4b).
  custom.ai.hermes = {
    enable = true;
    ollamaUrl = "http://${laptopIP}:11434/v1";
    apiKeyFile = config.sops.templates."hermes-env".path;
    vaultPath = vaultPath;
    soul = builtins.readFile ./SOUL.md;
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

  # VM-specific ordering: the vault must be cloned before the agent loads
  # the vault MCP server. Other hermes-agent service tweaks (Env, ExecStartPre,
  # TimeoutStopSec) come from the AI module.
  systemd.services.hermes-agent = {
    requires = [ "vault-sync.service" ];
    after = [ "vault-sync.service" ];
  };

  # sops-nix: this VM decrypts its own secrets using its ed25519 SSH host
  # key as the age identity (sops-nix converts ssh-ed25519 keys to age
  # automatically). The host key lives on /var/lib/hermes and is also
  # used as the SSH identity, so there's a single key to manage.
  # The encrypted file lives in dotfiles-private/hosts/personal-agent/secrets.yaml.
  sops = {
    defaultSopsFile = private.sopsSecretsFile;
    age.sshKeyPaths = [ "/var/lib/hermes/.ssh-host-keys/ssh_host_ed25519_key" ];
    secrets."hermes-agent/api-server-key" = { };
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

}
