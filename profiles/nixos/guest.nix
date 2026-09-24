# microVM guest kind: base + the operator account. The sealed-guest mechanism is modules/nixos/microvm/guest.nix.
{ fleet, ... }:
{
  imports = [ ./base.nix ];

  users.users.bphenriques = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keys = fleet.ssh.authorizedKeys;
  };
  security.sudo.wheelNeedsPassword = false;   # guests carry no passwords (users.mutableUsers = false)
}
