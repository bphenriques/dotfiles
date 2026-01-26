{ self, config, ... }: {
  imports = [
    ../compute/services/pocket-id
  ];

  custom.home-server = {
    enable = true;
    domain = self.settings.laptop.domain;
    cloudflareEmail = self.settings.cloudflareEmail;
  };

  custom.home-server.users.bphenriques = self.settings.users.bphenriques // {
    firstName = "Bruno";
    lastName = "Henriques";
    services = {
      pocket-id = {
        enable = true;
        groups = [ "admins" "users" ];
      };
      immich.enable = true;
      obsidian-livesync = {
        enable = true;
        passwordFile = config.sops.secrets.obsidian_livesync_bphenriques_password.path;
        databases = [ "obsidiandb-bphenriques" ];
      };
    };
  };
}
