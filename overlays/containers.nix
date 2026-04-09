# Run `nix run .#check-updates` to check for newer upstream releases.
_final: prev: let
  lib = prev.lib;
  images = {
    grist = {
      image = "gristlabs/grist";
      version = "1.7.12";
      updateInfo = { repo = "gristlabs/grist-core"; stripPrefix = "v"; };
    };
    larapaper = {
      image = "ghcr.io/usetrmnl/larapaper";
      version = "0.31.4";
      updateInfo = { repo = "usetrmnl/larapaper"; };
    };
    kapowarr = {
      image = "mrcas/kapowarr";
      version = "1.3.1";
      updateInfo = { repo = "Casvt/Kapowarr"; stripPrefix = "V"; };
    };
    romm = {
      image = "rommapp/romm";
      version = "4.8.1";
      updateInfo = { repo = "rommapp/romm"; };
    };
  };
in {
  containerImages = images;
  trackedContainerVersions = lib.mapAttrsToList (name: img: {
    inherit name;
    inherit (img) version;
    repo = img.updateInfo.repo;
    stripPrefix = img.updateInfo.stripPrefix or "";
  }) images;
}
