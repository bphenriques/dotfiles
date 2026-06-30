{ config, pkgs, inputs, private, shareVm, ... }:
let
  inherit (shareVm) dataRoot;
  sshHostKey = "${dataRoot}/.ssh-host-keys/ssh_host_ed25519_key";
in
{
  # sops's age identity is the VM's SSH host key, on the state volume — so the volume joins
  # the initrd (neededForBoot) and an activation step generates the key (if absent) before
  # sops's own runs. One clean pass, first boot included; the bootstrap is in ./README.md.
  fileSystems.${dataRoot}.neededForBoot = true;
  sops = {
    defaultSopsFile = private.sopsSecretsFile;
    age.sshKeyPaths = [ sshHostKey ];
    secrets."tailscale/authkey" = { };
  };
  system.activationScripts = {
    sshHostKeyInit.text = ''
      if [ ! -e ${sshHostKey} ]; then
        install -d -m 700 ${dataRoot}/.ssh-host-keys
        ${pkgs.openssh}/bin/ssh-keygen -t ed25519 -N "" -f ${sshHostKey}
      fi
    '';
    setupSecrets.deps = [ "sshHostKeyInit" ];
  };
}
