{ lib, config, ... }:
let
  smbCfg = config.selfhost.storage.mounts.smb;
in
{
  programs.zsh = {
    enable = true;
    enableGlobalCompInit = false;   # home-manager runs compinit; twice doubles startup.
  };

  sops.secrets."users/bphenriques/hashedPassword".neededForUsers = true;
  users.users.bphenriques = {
    isNormalUser = true;
    uid = 1000;
    shell = config.programs.zsh.package;
    hashedPasswordFile = config.sops.secrets."users/bphenriques/hashedPassword".path; # mkpasswd --method=SHA-512 --stdin
    extraGroups = [ "wheel" ]
      ++ lib.optionals config.networking.networkmanager.enable  [ "networkmanager" ]
      ++ lib.optionals config.virtualisation.docker.enable      [ "docker" ]
      ++ lib.optionals config.services.sunshine.enable          [ "input" ]
      ++ lib.optionals smbCfg.enable (lib.mapAttrsToList (_: mount: mount.group) smbCfg.shares);

    openssh.authorizedKeys.keys = config.fleet.ssh.authorizedKeys ++ [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIF7T3w1ex/9s2FBy8EM0HTSjF0kQIQi61ZwBh/mXQyNv ios-terminus"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAU2QHSKkqYS+FE5jYLvcf2Yq8gvd60EoI6xtCaNYeFZ bruno.henriques@iterable"
    ];
  };

  programs.git = {
    enable = true;
    config.safe.directory = let inherit (config.users.users.bphenriques) home; in [
      "${home}/.dotfiles"
      "${home}/.dotfiles-private"
    ];
  };

  home-manager.users.bphenriques = import ./home.nix;
}
