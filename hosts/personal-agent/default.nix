# personal-agent — guest-side NixOS configuration for the assistant microvm.
# Runs the assistant runtime (currently hermes-agent) as a service user with
# no shell or sudo. Human admin access is via the `bphenriques` account,
# reachable over SSH (ProxyJump through compute) on the bridge IP.
{ config, inputs, self, private, ... }:
let
  fleet = import ../shared.nix;
in
{
  imports = [
    inputs.microvm.nixosModules.microvm
    # mkMicrovmGuest deliberately doesn't load all self.nixosModules
    # (would pull in homelab too); pick what we need explicitly.
    self.nixosModules.ai-hermes-agent   # pulls in inputs.hermes-agent transitively
    self.nixosModules.fleet-shared      # `custom.fleet.*` options
    ./microvm.nix
    ./vault.nix
  ];

  networking.hostName = "personal-agent";
  system.stateVersion = "26.05";

  custom.fleet = import ../shared.nix;

  time.timeZone = "Europe/Lisbon";
  i18n.defaultLocale = "en_US.UTF-8";
  console.keyMap = "pt-latin1";

  # SSH is for the human admin (bphenriques), not for the agent. Reachable only
  # via ProxyJump through compute. Host keys persist on the volume so the
  # laptop's known_hosts survives rebuilds.
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
    hostKeys = [
      { path = "/var/lib/hermes/.ssh-host-keys/ssh_host_ed25519_key"; type = "ed25519"; }
    ];
  };

  # Only bphenriques can perform rootful operations. The hermes service user
  # (created by the upstream module) stays minimal — no shell, sudo, or SSH.
  # Passwordless sudo is acceptable: SSH is key-only and the VM is unreachable
  # from outside compute's bridge.
  users.users.bphenriques = {
    isNormalUser = true;
    extraGroups = [ "wheel" "systemd-journal" ];
    openssh.authorizedKeys.keys = fleet.authorizedSSHKeys;
  };
  security.sudo.wheelNeedsPassword = false;

  # Hermes runtime. Traefik routing lives on compute (services/hermes-api.nix);
  # the VM exposes the raw OpenAI-compatible API. Model defaults to fleet
  # config; vaultPath comes from ./vault.nix.
  custom.ai.hermes-agent = {
    enable = true;
    ollamaUrl = "http://${fleet.lan.hosts.laptop}:11434/v1";
    apiKeyFile = config.sops.templates."hermes-env".path;
    soul = builtins.readFile ./SOUL.md;
  };

  # sops activation runs in initrd, but /var/lib/hermes (where the SSH host key
  # sops uses as its age identity lives) isn't mounted yet. Re-run activation
  # in stage 2 once local-fs is up; activation is idempotent.
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

  # sops-nix uses the VM's ed25519 SSH host key as the age identity
  # (sops-nix converts ssh-ed25519 → age automatically). Same key as SSH login.
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
