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
    # Image tags carry a `-cli` suffix the GitHub release tags do not, so consumers append it.
    livesync-cli = {
      image = "ghcr.io/vrtmrz/livesync-cli";
      version = "1.0.29";
      homepage = "https://github.com/vrtmrz/obsidian-livesync";
      updateInfo = { repo = "vrtmrz/obsidian-livesync"; };
    };
    papra = {
      image = "ghcr.io/papra-hq/papra";
      version = "26.6.1";
      homepage = "https://github.com/papra-hq/papra";
      updateInfo = { repo = "papra-hq/papra"; stripPrefix = "@papra/app@"; };
    };
    # Toolbox image, so `Cmd` is a bare shell; `hosts/ai/services/comfyui.nix` supplies the launch line.
    # No `updateInfo`: upstream cuts no GitHub releases, only datestamped tags, so nothing can track it.
    comfyui = {
      image = "docker.io/kyuz0/amd-strix-halo-comfyui";
      version = "20260811-102353";
      homepage = "https://github.com/kyuz0/amd-strix-halo-comfyui-toolboxes";
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
  }) (lib.filterAttrs (_: img: img ? updateInfo) images);
}
