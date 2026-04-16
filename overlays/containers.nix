# Run `nix run .#check-updates` to check for newer upstream releases.
_final: prev: let
  lib = prev.lib;
  images = {
    grist = {
      image = "gristlabs/grist";
      version = "1.7.12";
      homepage = "https://github.com/gristlabs/grist-core";
      updateInfo = { repo = "gristlabs/grist-core"; stripPrefix = "v"; };
    };
    larapaper = {
      image = "ghcr.io/usetrmnl/larapaper";
      version = "0.32.1";
      homepage = "https://github.com/usetrmnl/larapaper";
      updateInfo = { repo = "usetrmnl/larapaper"; };
    };
    kapowarr = {
      image = "mrcas/kapowarr";
      version = "1.3.1";
      homepage = "https://github.com/Casvt/Kapowarr";
      updateInfo = { repo = "Casvt/Kapowarr"; stripPrefix = "V"; };
    };
    papra = {
      image = "ghcr.io/papra-hq/papra";
      version = "26.4.0";
      homepage = "https://github.com/papra-hq/papra";
      updateInfo = { repo = "papra-hq/papra"; stripPrefix = "@papra/app@"; };
    };
    romm = {
      image = "rommapp/romm";
      version = "4.8.1";
      homepage = "https://github.com/rommapp/romm";
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
