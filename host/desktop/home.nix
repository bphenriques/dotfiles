{ config, pkgs, lib, ... }:
{
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

    # Media

    # Take a look at:
    # https://beets.readthedocs.io/en/stable/plugins/convert.html
    # https://beets.readthedocs.io/en/stable/guides/advanced.html#automatically-add-new-music-to-your-library
    # https://beets.readthedocs.io/en/stable/guides/advanced.html#useful-reports
    programs.beets = {
      enable = true;
      settings = {
        directory = "/mnt/data/Media/Music";
        library = "/mnt/data/Media/music-beets-library.db";

        # More on that here: https://beets.readthedocs.io/en/stable/plugins/index.html
        # - https://beets.readthedocs.io/en/stable/plugins/sonosupdate.html
        # - https://beets.readthedocs.io/en/stable/plugins/subsonicupdate.html
        # chroma requires chromaprint and pyaccoustic
        plugins = "web fetchart lastgenre chroma spotify";  # web in http://localhost:8337 maybe embedart? or lyrics?
        paths = {
          default = "$albumartist/$album%aunique{}/$track $title";
          singleton = "Non-Album/$artist/$title";
          comp = "Compilations/$album%aunique{}/$track $title";
        };
        musicbrainz = {
          extra_tags = ["date" "year" "originalyear" "originalartist" "originalalbum" "artists"]; # lyrics
        };
      };
    };

    # Fonts
    fonts.fontconfig.enable = true;
    home.packages = with pkgs; [
      chromaprint           # Identify audios
      python311Packages.pyacoustid
      unrar
      xclip
      python3
      rofi
      (nerdfonts.override { fonts = [ "Hack" ]; })
    ];
  };
}
