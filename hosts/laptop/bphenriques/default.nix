{ lib, config, self, ... }:
let
  smbCfg = config.custom.homelab.smb;
in
{
  sops.secrets."users/bphenriques/hashedPassword".neededForUsers = true;
  users.users.bphenriques = {
    isNormalUser = true;
    uid = 1000;
    hashedPasswordFile = config.sops.secrets."users/bphenriques/hashedPassword".path; # mkpasswd --method=SHA-512 --stdin
    extraGroups = [ "wheel" ]
      ++ lib.optionals config.networking.networkmanager.enable  [ "networkmanager" ]
      ++ lib.optionals config.virtualisation.docker.enable      [ "docker" ]
      ++ lib.optionals config.services.sunshine.enable          [ "input" ]
      ++ lib.optionals (smbCfg.enable && smbCfg.mounts ? media) [ smbCfg.mounts.media.group ]
      ++ lib.optionals (smbCfg.enable && smbCfg.mounts ? bphenriques) [ smbCfg.mounts.bphenriques.group ];

    openssh.authorizedKeys.keys = self.shared.authorizedSSHKeys;
  };

  programs.git = {
    enable = true;
    config.safe.directory = [
      "/home/bphenriques/.dotfiles"
      "/home/bphenriques/.dotfiles-private"
    ];
  };

  home-manager.users.bphenriques = import ./home.nix;
}
