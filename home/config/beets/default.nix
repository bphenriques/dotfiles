{ config, lib, pkgs, ... }:
{
    # TODO: Can we check if these paths exist?
    assertions = [
      {
         assertion = config.programs.beets.settings ? directory && config.programs.beets.settings ? library;
         message = "programs.beets.settings.directory and programs.beets.settings.library must be set and pointing to a valid path.";
      }
   ];

   programs.beets = {
     enable = true;
     settings = {
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
}
