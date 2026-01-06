{ privateSettings, ...}: {
  imports = [
    ./pocket-id.nix
    ./miniflux.nix
  ];

  custom.home-server = {
    enable = true;
    domain = privateSettings.personalDomain.domain;
    cloudflareEmail = privateSettings.personalDomain.email;
  };
}