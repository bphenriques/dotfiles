{ config, pkgs, lib, ... }:
{
  # Music Library Manager
  home = {
    programs.beets = {
      enable = true;
      settings = {
        directory = config.user.musicDir;
        library = "/mnt/data/Media/Music/beets-library.db";

        paths = {
          default = "$albumartist/$album%aunique{}/$track $title";
          singleton = "$artist/Non-Album/$title";
          comp = "Compilations/$album%aunique{}/$track $title";
        };

        musicbrainz = {
          extra_tags = ["date" "year" "originalyear" "originalartist" "originalalbum" "artists"];
        };

        # Docs: https://beets.readthedocs.io/en/stable/plugins/index.html
        plugins = let
          providers = ["chroma" "spotify" "deezer"];
          metadata  = ["fetchart" "embedart" "lyrics"];
          utility   = ["edit" "duplicates" "scrub"];
         in builtins.concatStringsSep " " (providers ++ metadata ++ utility);
      };
    };

    home.packages = with pkgs; [
      python3                           # Required by most plugins
      chromaprint                       # Required by chroma
      python311Packages.pyacoustid      # Required by chroma
      python39Packages.requests         # Required by lyrics
      python310Packages.beautifulsoup4  # Required by lyrics
    ];
  };
}
