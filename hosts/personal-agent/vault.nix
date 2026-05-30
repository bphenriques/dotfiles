# Vault: depth-1 SSH clone of bphenriques/notes from gitea, refreshed every 5
# minutes. The agent reads (mode 0750 root:hermes); writes are not supported on
# this path — see voice-memo-pipeline.md for the PR-based write flow.
{ config, pkgs, inputs, ... }:
let
  vaultPath = "/var/lib/hermes/vault";
  homelabDomain = inputs.dotfiles-private.hosts.compute.settings.domain;
  vaultRepoUrl = "ssh://gitea@git.${homelabDomain}:2222/bphenriques/notes.git";

  # ssh wrapper: pin identity to the VM's host key (also the sops age key —
  # single trust anchor), persist known_hosts on the volume, accept gitea's
  # host key on first contact.
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
        # Re-point on every sync so a Nix-level URL change flows into
        # .git/config without manual surgery.
        git -C ${vaultPath} remote set-url origin ${vaultRepoUrl}
        git -C ${vaultPath} fetch --quiet --depth 1 origin
        git -C ${vaultPath} reset --hard --quiet FETCH_HEAD
      else
        git clone --quiet --depth 1 ${vaultRepoUrl} ${vaultPath}
      fi
      # Re-apply on every sync — git can create new files/dirs with default
      # perms; we want them readable by the hermes group only.
      chown -R root:hermes ${vaultPath}
      chmod -R u=rwX,g=rX,o= ${vaultPath}
    '';
  };
in
{
  # Hand the path to the AI module so the vault MCP server points at it.
  custom.ai.hermes-agent.vaultPath = vaultPath;

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

  # The agent loads the vault MCP server at startup — vault must be on disk
  # before then. Other hermes-agent service tweaks come from the AI module.
  systemd.services.hermes-agent = {
    requires = [ "vault-sync.service" ];
    after = [ "vault-sync.service" ];
  };
}
