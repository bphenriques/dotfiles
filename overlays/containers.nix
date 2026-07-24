# Run `nix run .#check-updates` to check for newer upstream releases.
_final: prev: let
  inherit (prev) lib;
  images = {
    kapowarr = {
      image = "mrcas/kapowarr";
      version = "1.3.1";
      homepage = "https://github.com/Casvt/Kapowarr";
      updateInfo = { repo = "Casvt/Kapowarr"; stripPrefix = "V"; };
    };
    papra = {
      image = "ghcr.io/papra-hq/papra";
      version = "26.6.1";
      homepage = "https://github.com/papra-hq/papra";
      updateInfo = { repo = "papra-hq/papra"; stripPrefix = "@papra/app@"; };
    };
    romm = {
      image = "rommapp/romm";
      version = "5.0.0";
      homepage = "https://github.com/rommapp/romm";
      updateInfo = { repo = "rommapp/romm"; };
    };
    nextchat = {
      image = "yidadaa/chatgpt-next-web";
      version = "2.16.1";
      homepage = "https://github.com/ChatGPTNextWeb/NextChat";
      updateInfo = { repo = "ChatGPTNextWeb/NextChat"; stripPrefix = "v"; };
    };
  };
in {
  containerImages = images;
  trackedContainerVersions = lib.mapAttrsToList (name: img: {
    inherit name;
    inherit (img) version;
    inherit (img.updateInfo) repo;
    stripPrefix = img.updateInfo.stripPrefix or "";
  }) images;
}
