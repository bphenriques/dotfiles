# The sealed guest can't reach gitea, so compute clones its local bare repo into a dir agent-vm mounts RO.
{ config, pkgs, ... }:
let
  vaultDir = "/var/lib/agent-vm-vault";   # virtiofs source (agent-vm/microvm.nix)
  sourceRepo = "${config.services.gitea.repositoryRoot}/bphenriques/notes.git"; # vault must exist as bphenriques/notes
in
{
  systemd.services.agent-vm-vault-sync = {
    description = "Refresh the read-only Obsidian vault shared into agent-vm";
    after = [ "gitea.service" ];
    path = [ pkgs.git pkgs.coreutils ];
    serviceConfig.Type = "oneshot";
    # Runs as root: gitea's bare repos are gitea-owned 0700, and root must read them.
    script = ''
      set -euo pipefail
      if [ ! -e ${sourceRepo} ]; then
        echo "vault source ${sourceRepo} missing; create the gitea repo bphenriques/notes" >&2
        exit 0
      fi
      if [ -d ${vaultDir}/.git ]; then
        git -C ${vaultDir} fetch --quiet --depth 1 origin
        git -C ${vaultDir} reset --hard --quiet FETCH_HEAD
      else
        git clone --quiet --depth 1 "file://${sourceRepo}" ${vaultDir}
      fi
      # virtiofs passes UIDs through (no idmap), so world-read lets the guest's hermes user see it.
      chmod -R a+rX ${vaultDir}
    '';
  };

  systemd.timers.agent-vm-vault-sync = {
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnBootSec = "2min";
      OnUnitActiveSec = "5min";
      Persistent = true;
    };
  };

  # Must exist before the guest boots; virtiofsd fails on a missing source.
  systemd.tmpfiles.rules = [ "d ${vaultDir} 0755 root root -" ];
}
