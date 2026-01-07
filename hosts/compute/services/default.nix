{ self, ...}: {
  imports = [
    ./pocket-id.nix
    #./miniflux.nix
  ];

  custom.home-server = {
    enable = true;
    domain = self.settings.personalDomain.domain;
    cloudflareEmail = self.settings.personalDomain.email;
  };
}