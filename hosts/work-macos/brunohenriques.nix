{ pkgs, private, ... }:
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

  my.dotfiles.enable = true;
  programs.fish.plugins = [
    { name = "nvm"; inherit (pkgs.fishPlugins.nvm) src; }
  ];
  programs.git.includes = [{
    condition = "hasconfig:remote.*.url:git@github.com:${private.settings.git.org}/**";
    contents.user.email = private.settings.git.email;
  }];

  programs.awscli.enable = true;
  programs.pyenv.enable = true;
  programs.ssh.includes = [ "$HOME/.ssh/iterable-config.local" ];

  home.stateVersion = "26.05";
}
