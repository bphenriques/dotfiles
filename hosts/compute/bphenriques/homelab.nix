{ config, ... }:
{
  custom.home-server.users.bphenriques = {
    email = "bphenriques@example.com";
    name = "Bruno Henriques";
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
  sops.secrets.obsidian_livesync_bphenriques_password = { };
}