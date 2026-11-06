{ config, pkgs, private, ... }:
{
  imports = [
    ../../profiles/home-manager/base.nix
    ../../profiles/home-manager/desktop
    ../../profiles/home-manager/development
    ./sdkman.nix
  ];

  home.packages = [
    # Generic
    pkgs.go-task
    pkgs.wget
    pkgs.fnm               # Node versions, reads .nvmrc

    # Infrastructure / Gitops
    pkgs.kubectl
    pkgs.kubectx

    # FIXME:
    # readline -> better experience for python repl. Will skip for now.
    # openssl -> MacOS already bundleds.
    # xz
    # zlib
    # imagemagick
  ];

  programs.nh.darwinFlake = "${config.programs.nh.flake}#work-macos";   # the work-managed hostname must stay
  programs.zsh.initContent = ''eval "$(fnm env --use-on-cd --shell zsh)"'';
  programs.git.includes = [{
    condition = "hasconfig:remote.*.url:git@github.com:${private.settings.git.org}/**";
    contents.user.email = private.settings.git.email;
  }];

  programs.awscli.enable = true;
  programs.pyenv.enable = true;
  programs.ssh.includes = [ "$HOME/.ssh/iterable-config.local" ];

  home.stateVersion = "26.05";
}
