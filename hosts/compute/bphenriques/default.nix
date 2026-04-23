{ lib, config, self, ... }:
let
  smbCfg = config.custom.homelab.smb;
in
{
  sops.secrets."users/bphenriques/hashedPassword".neededForUsers = true;
  users.users.bphenriques = {
    isNormalUser = true;
    uid = 1000;
    hashedPasswordFile = config.sops.secrets."users/bphenriques/hashedPassword".path;
    extraGroups = [ "wheel" ]
      ++ lib.optionals (smbCfg.enable && smbCfg.mounts ? media) [ smbCfg.mounts.media.group ]
      ++ lib.optionals (smbCfg.enable && smbCfg.mounts ? bphenriques) [ smbCfg.mounts.bphenriques.group ];

    openssh.authorizedKeys.keys = self.shared.authorizedSSHKeys;
  };

  home-manager.users.bphenriques = import ./home.nix;
}
