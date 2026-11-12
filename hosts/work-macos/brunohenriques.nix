{ lib, config, osConfig, pkgs, private, ... }:
let
  branchPrefix = "dpt";
  # Only new branches are checked, so pushing to main or a colleague's branch is never blocked.
  branchNameHook = pkgs.writeShellApplication {
    name = "git-branch-name-hook";
    text = ''
      while read -r _ _ ref remote_oid; do
        if [[ $remote_oid =~ ^0+$ && $ref == refs/heads/* && ! $ref =~ ^refs/heads/${branchPrefix}-[0-9]+-[a-z0-9-]+$ ]]; then
          echo "pre-push: $ref must match ${branchPrefix}-<number>-<slug>" >&2
          exit 1
        fi
      done
    '';
  };
in
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

    # Other
    pkgs.acli              # acli jira auth login --web
  ];

  programs.nh.darwinFlake = "${config.programs.nh.flake}#work-macos";   # the work-managed hostname must stay
  programs.zsh.initContent = ''eval "$(fnm env --use-on-cd --shell zsh)"'';

  # AI Agent
  home.shellAliases.agent = "cursor-agent"; # The cursor-cli cask only links cursor-agent

  programs.git.includes = [{
    condition = "hasconfig:remote.*.url:git@github.com:${private.settings.git.org}/**";
    contents.user.email = private.settings.git.email;
    contents.hook.branch-name = {
      event = "pre-push";
      command = lib.getExe branchNameHook;
    };
  }];

  # Other
  programs.awscli.enable = true;
  programs.pyenv.enable = true;
  programs.ssh.includes = [ "$HOME/.ssh/iterable-config.local" ];
  programs.ssh.settings.laptop = {
    HostName = osConfig.fleet.lan.hosts.laptop;
    User = "bphenriques";
  };

  home.stateVersion = "26.05";
}
