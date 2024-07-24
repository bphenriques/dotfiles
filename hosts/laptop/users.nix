{ config, pkgs, lib, ... }:
{
  # TODO: check https://github.com/iynaix/dotfiles/blob/main/nixos/users.nix
  users = {
    mutableUsers = false;
    users = {
      bphenriques = {
        isNormalUser = true;
        hashedPasswordFile = config.sops.secrets.bphenriques_password.path;
        description = "bphenriques";
        extraGroups = [ "wheel" "networkmanager" "docker" ];
      };
    };
  };
  users.users.bphenriques.shell = pkgs.fish;  # Fish is managed in Home-Manager. Keeping the default shell for root.

  home-manager.users.bphenriques = { pkgs, ... }: {
    imports = [
      ../../home
      ../../home/plasma
    ];
    programs.plasma.workspace.wallpaper = ./wallpaper.png;
    programs.firefox.profiles.default.bookmarks = import ./secrets/bookmarks.age.nix;
    # programs.beets.settings.directory = config.user.musicDir;


    # Gpg
    programs.gpg.enable = true;
    services.gpg-agent = {
      enable = true;
      pinentryPackage = pkgs.pinentry-gnome3;
    };

    home.packages = with pkgs; [
      killall     # Useful
    ];

    xdg.userDirs.enable = true;
    xdg.userDirs.createDirectories = false;
    xdg.mimeApps.enable = true; # TODO: Create associations?

    custom.dotfiles.privateSSHPath = "/persist/config/bphenriques/home/bphenriques/.ssh/id_ed25519";
    custom.dotfiles.host = "laptop";

    # TODO: should I enable https://github.com/NixOS/nixpkgs/issues/160923 ?
    # xdg.portal.enable = true;   # TODO: https://github.com/flatpak/xdg-desktop-portal. Should I set xdgOpenUsePortal?

    home.stateVersion = "24.05";
  };
}

# TODO: https://search.nixos.org/options?channel=unstable&show=networking.networkmanager.ensureProfiles.profiles&from=0&size=200&sort=relevance&type=packages&query=networking.networkmanager.ensureProfiles
