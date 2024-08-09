{ pkgs, lib, ... }:
{
  home.packages = [
    (pkgs.lutris.override {
      extraPkgs = pkgs: [
        pkgs.kdialog   # Required otherwise installation might not work. FIXME: may only make sense for KDE
        pkgs.protobuf  # Required for battlenet.
      ];
    })
  ];

  home.persistence = lib.mkIf config.custom.impermanence.enable {
    "${config.custom.impermanence.configLocation}".directories = [ ".config/lutris" ".local/share/lutris" ];
  };
}
