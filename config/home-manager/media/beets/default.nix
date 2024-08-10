{ config, lib, pkgs, ... }:
{
    assertions = [
      {
         assertion = config.programs.beets.settings ? directory;
         message = "programs.beets.settings.directory must be set and point to a valid directory.";
      }
   ];

   programs.beets = {
     enable = true;
     package = pkgs.beets-unstable; # Github version
     settings = {
       library = "${config.xdg.dataHome}/beets.db"; # FIXME: Can't live in the NAS due to intermetient DB lock issues. TODO: Backup to NAS. Perhaps https://serverfault.com/questions/146961/automatically-run-a-command-every-time-a-file-is-changed ?
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
         metadata  = ["fetchart" "embedart" "lyrics" "mbsync"];
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

   # Fix when moving the files around
   # https://github.com/beetbox/beets/issues/133
   # sqlite3 beets.db "UPDATE items SET path = replace(path, '/home/bphenriques/Media/Music', '/home/bphenriques/Media/music');"
   # sqlite3 beets.db "UPDATE albums SET artpath = replace(artpath, '/home/bphenriques/Media/Music', '/home/bphenriques/Media/music');"
}
