{ self, ... }:
{
  custom.home-server.users.bphenriques = self.settings.users.bphenriques // {
    services = {
      pocket-id = {
        enable = true;
        groups = [ "admins" "users" ];
      };
      immich.enable = true;
      couchdb = {
        enable = true;
        databases = [ "obsidiandb-bphenriques" ];
      };
    };
  };
}