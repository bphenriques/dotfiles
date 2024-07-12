{ lib, pkgs, config, ... }:

with lib;
let
  cfg = config.custom.dotfiles;
in
{
  options.custom.dotfiles = {
    enable = mkEnableOption "Whether to enable repository.";
    directory = mkOption {
      type = with lib.types; str;
      description = "Location of the dotfiles repository";
      default = "${config.home.homeDirectory}/.dotfiles";
    };

    host = mkOption {
      type = with lib.types; str;
      description = "Dotfiles host identifier";
    };

    branch = mkOption {
      type = with lib.types; str;
      description = "Default branch to clone";
      default = "main";
    };

    remoteSSHRepository = mkOption {
      type = types.str;
      description = "Remote location of the dotfiles repository in SSH format";
      default = "git@github.com:bphenriques/dotfiles.git";
    };

    privateSSHPath = mkOption {
      type = types.str;
      description = "Path to the to the private SSH key that has permissions to clone the repository using SSH credentials.";
      default = "${config.home.homeDirectory}/.ssh/id_ed25519";
    };

    publicSSHPath = mkOption {
      type = types.str;
      description = "Path to the to the public SSH key that has permissions to clone the repository using SSH credentials.";
      default = "${cfg.privateSSHPath}.pub";
    };
  };

  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = !builtins.pathExists cfg.privateSSHPath || !builtins.pathExists cfg.publicSSHPath;
        message = "SSH Keys are not set. Either the public or the private keys are present.";
      }
    ];

    home.activation.clone-dotfiles = lib.mkAfter ''
      if [ ! -d "${cfg.directory}" ]; then
        tmp="$(mktemp -d)"
        ${pkgs.git}/bin/git clone -b ${cfg.branch} ${cfg.remoteSSHRepository} "${cfg.directory}"
      else
        if ! git status "${cfg.directory}"; then
          echo "${cfg.directory} already exists but does not contain a repository inside"
          ${pkgs.git}/bin/git clone -b ${cfg.branch} `${cfg.remoteSSHRepository}` "${cfg.directory}"
        else
          echo "Skipping dotfiles setup as ${cfg.directory} already exists!"
        fi
      fi
    '';
  };
}
