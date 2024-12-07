{ pkgs, self, ... }:
let
  wallpapers = self.private.wallpapers.override {
    selected = [ "lake-fishing-sunset" "mountains" "whale-sunset" "watch-tower" ];
  };
in
{
  # Login Screen
  services.displayManager.sddm = {
    enable = true;
    wayland.enable = true;
    theme = "sddm-astronaut-theme";
  };
  environment.systemPackages = [
    # https://github.com/Keyitdev/sddm-astronaut-theme/blob/master/theme.conf
    # It is possible to override the package and set themeConfig. For now, I will iterate like this.
    pkgs.sddm-astronaut
    (pkgs.writeTextDir "share/sddm/themes/sddm-astronaut-theme/theme.conf.user" ''
      [General]
      background=${wallpapers}/share/wallpapers/watch-tower.png
      FullBlur="false"
      PartialBlur="false"
      FormPosition="center"
    '')
   ];
}
