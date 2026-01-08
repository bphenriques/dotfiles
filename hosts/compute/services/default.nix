{ self, ...}: {
  imports = [
    ./pocket-id.nix
    ./miniflux.nix
    ./prowlarr.nix
  ];

  custom.home-server = {
    enable = true;
    domain = self.settings.compute.domain;
    cloudflareEmail = self.settings.cloudflareEmail;
  };
}