# Run `nix run .#check-updates` to check for newer upstream releases.
_final: prev: let
  inherit (prev) lib;
  images = {
    cleanuparr = {
      image = "ghcr.io/cleanuparr/cleanuparr";
      version = "2.10.9";
      homepage = "https://github.com/Cleanuparr/Cleanuparr";
      updateInfo = { repo = "Cleanuparr/Cleanuparr"; stripPrefix = "v"; };
    };
    kapowarr = {
      image = "docker.io/mrcas/kapowarr";
      version = "1.3.2";
      tagPrefix = "v";   # Release tags are `V1.3.2`, image tags `v1.3.2`.
      homepage = "https://github.com/Casvt/Kapowarr";
      updateInfo = { repo = "Casvt/Kapowarr"; stripPrefix = "V"; };
    };
    livesync-cli = {
      image = "ghcr.io/vrtmrz/livesync-cli";
      version = "1.0.32";
      tagSuffix = "-cli";
      homepage = "https://github.com/vrtmrz/obsidian-livesync";
      updateInfo = { repo = "vrtmrz/obsidian-livesync"; };
    };
    papra = {
      image = "ghcr.io/papra-hq/papra";
      version = "26.6.2";
      tagSuffix = "-rootless";
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
    ollama = {
      image = "docker.io/ollama/ollama";
      version = "0.40.2";
      tagSuffix = "-rocm";   # The variant that carries the AMD GPU runtime.
      homepage = "https://github.com/ollama/ollama";
      updateInfo = { repo = "ollama/ollama"; stripPrefix = "v"; };
    };
  };
  tagged = lib.mapAttrs (_: img: let
    tagPrefix = img.tagPrefix or "";
    tagSuffix = img.tagSuffix or "";
  in img // { inherit tagPrefix tagSuffix; tag = "${tagPrefix}${img.version}${tagSuffix}"; }) images;
in {
  containerImages = tagged;
  # check-updates resolves each release against the registry, so it needs the full tag shape.
  trackedContainerVersions = lib.mapAttrsToList (name: img: {
    inherit name;
    inherit (img) version image tagPrefix tagSuffix;
    inherit (img.updateInfo) repo;
    stripPrefix = img.updateInfo.stripPrefix or "";
  }) (lib.filterAttrs (_: img: img ? updateInfo) tagged);
}
