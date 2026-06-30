{ config, pkgs, inputs, private, shareVm, ... }:
let
  fleet = import ../shared.nix;
  inherit (shareVm) dataRoot;
  sshHostKey = "${dataRoot}/.ssh-host-keys/ssh_host_ed25519_key";
  adminUser = "bphenriques";
in
{
  imports = [
    ./settings.nix
    inputs.microvm.nixosModules.microvm
    ./microvm.nix
    ./firewall.nix
    ./services
    ./secrets.nix
  ];

  time.timeZone = "Europe/Lisbon";
  i18n.defaultLocale = "en_US.UTF-8";

  users.users.${adminUser} = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = fleet.ssh.authorizedKeys;
  };
  security.sudo.wheelNeedsPassword = false;

  system.stateVersion = "26.05";
}
