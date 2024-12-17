{ pkgs, lib, config, ... }:
let
  inherit (lib) foldl';

  musicLibrary = "${config.xdg.userDirs.music}/library";
  playlistsDirectory = "${config.xdg.userDirs.music}/playlists";

  # Beets require absolute paths: https://github.com/beetbox/beets/issues/133
  # If needed:
  # 1. Find the base path in the db: beet list -f '$path' artist:beatles | head -n 1
  # 2. Manually update the database:
  #   sqlite3 $XDG_DATA_HOME/beets/library.db "UPDATE items SET path = replace(path, '/home/bphenriques/Music/Library', '/home/bphenriques/music/library');"
  #   sqlite3 $XDG_DATA_HOME/beets/library.db "UPDATE albums SET artpath = replace(artpath, '/home/bphenriques/Music/Library', '/home/bphenriques/music/library');"
  #
  # 3. Confirm if everything is alright. The following command should not hint that files should be deleted.
  #   beets update -p
  database = "${config.xdg.dataHome}/beets/library.db";
  databaseBackup = "${config.xdg.userDirs.music}/beets.db.backup";

  # Healthcheck: beet bad && beet duplicates
  # Update files: beet fetchart && beet fingerprint && beet embedart && beet scrub
  # Docs: https://beets.readthedocs.io/en/stable/plugins/index.html
  plugins = let
    providers = [ "chroma" "spotify" "deezer" ];
    metadata  = [ "fetchart" "embedart" "lyrics" "mbsync" ]; # lastgenre
    health    = [ "duplicates" "badfiles" ];
    utility   = [ "edit" "playlist" "scrub" "fish" ]; # https://beets.readthedocs.io/en/stable/plugins/smartplaylist.html
  in (providers ++ health ++ metadata ++ utility);
  basePackage = pkgs.beets-unstable.override {
    # Reference: https://github.com/NixOS/nixpkgs/blob/master/pkgs/tools/audio/beets/builtin-plugins.nix
    pluginOverrides = foldl' (acc: plugin: acc // { "${plugin}".enable = true; }) { } plugins;
  };

  # Sanity check + backup database file to NAS. Can't store the DB file in the NAS as it leads to lock issues.
  finalPackage = (pkgs.writeScriptBin "beet" ''
    #!${pkgs.stdenv.shell}
    if [ ! -d "${musicLibrary}" ]; then
      echo "${musicLibrary} does not exist!"
      exit 1
    fi

    ${lib.getExe basePackage} "$@"
    status=$?
    if [ $status -eq 0 ] && [ -f "${database}" ] && ([ ! -f "${databaseBackup}" ] || [[ "$(md5sum "${databaseBackup}")" = "$(md5sum "${database}")" ]]); then
      echo "Backing up beets library: ${database}"
      cp -f "${database}" "${databaseBackup}"
    fi
    exit $status
  '');
in
{
  programs.beets = {
    enable = pkgs.stdenv.isLinux;
    package = finalPackage;
    settings = {
      library = database;
      directory = musicLibrary;
      paths = {
        default = "$albumartist/$album%aunique{}/$track $title";
        singleton = "$artist/Non-Album/$title";
        comp = "Compilations/$album%aunique{}/$track $title";
      };
      plugins = builtins.concatStringsSep " " plugins;
      playlist = {
        auto = true;                        # Automatically remove/move items inside the playlists in case they move.
        relative_to = musicLibrary;
        playlist_dir = playlistsDirectory;
      };
      fetchart = {
        auto = true;
        cautious = true;
      };
      musicbrainz = {
        extra_tags = ["date" "year" "originalyear" "originalartist" "originalalbum" "artists"];
      };
    };
  };
}
