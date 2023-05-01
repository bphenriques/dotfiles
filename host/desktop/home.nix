{ config, pkgs, lib, ... }:
{
  # This is an alias (see custom module)
  home = {
    imports = [ ../../home ];

    # Programs
    programs.firefox.enable = true;
    services.dropbox.enable = true; # TODO: Change path but ensure that the folders

    # Gpg
    programs.gpg.enable = true;
    services.gpg-agent.enable = true;

    # Social
    modules.programs.discord.enable = true;

    # Fonts
    fonts.fontconfig.enable = true;
    home.packages = with pkgs; [
      rofi        # Launcher

      # Util
      unrar
      xclip
      (nerdfonts.override { fonts = [ "Hack" ]; })
    ];
  };

  imports = [ ./home-media.nix ];
}
