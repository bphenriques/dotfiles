{ self, ... }: {
  imports = [
    ../compute/services/pocket-id
  ];

  custom.home-server = {
    enable = true;
    domain = self.settings.laptop.domain;
    cloudflareEmail = self.settings.cloudflareEmail;
  };
}