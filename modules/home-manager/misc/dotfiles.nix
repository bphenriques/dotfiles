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
  };

  config = mkIf cfg.enable {
    home.activation.clone-dotfiles = lib.mkAfter ''
      if [ ! -d "${cfg.directory}" ]; then
        tmp="$(mktemp -d)"
        GIT_SSH_COMMAND="${pkgs.openssh}/bin/ssh -i ${cfg.privateSSHPath}" ${pkgs.git}/bin/git clone -b ${cfg.branch} ${cfg.remoteSSHRepository} "${cfg.directory}"
      else
        if ! ${pkgs.git}/bin/git status "${cfg.directory}"; then
          echo "${cfg.directory} already exists but does not contain a repository inside"
          GIT_SSH_COMMAND="${pkgs.openssh}/bin/ssh -i ${cfg.privateSSHPath}" ${pkgs.git}/bin/git clone -b ${cfg.branch} ${cfg.remoteSSHRepository} "${cfg.directory}"
        else
          echo "Skipping dotfiles setup as ${cfg.directory} already exists!"
        fi
      fi
      echo "${cfg.host}" > "${cfg.directory}/.nix-host"
    '';
  };
}
