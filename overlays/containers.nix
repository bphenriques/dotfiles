# Run `nix run .#check-updates` to check for newer upstream releases.
_final: prev: let
  inherit (prev) lib;
  images = {
    cleanuparr = {
      image = "ghcr.io/cleanuparr/cleanuparr";
      version = "2.10.2";
      homepage = "https://github.com/Cleanuparr/Cleanuparr";
      updateInfo = { repo = "Cleanuparr/Cleanuparr"; stripPrefix = "v"; };
    };
    kapowarr = {
      image = "docker.io/mrcas/kapowarr";
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
    nextchat = {
      image = "docker.io/yidadaa/chatgpt-next-web";
      version = "2.16.1";
      homepage = "https://github.com/ChatGPTNextWeb/NextChat";
      updateInfo = { repo = "ChatGPTNextWeb/NextChat"; stripPrefix = "v"; };
    };
    # The `ai` host appends `-rocm` to this tag; that variant is what carries the AMD GPU runtime.
    ollama = {
      image = "docker.io/ollama/ollama";
      version = "0.33.2";
      homepage = "https://github.com/ollama/ollama";
      updateInfo = { repo = "ollama/ollama"; stripPrefix = "v"; };
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
