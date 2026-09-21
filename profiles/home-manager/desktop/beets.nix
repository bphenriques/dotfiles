{ pkgs, lib, osConfig, config, ... }:
let
  inherit (lib) foldl';

  musicDir = "${osConfig.custom.shares.media.root}/music";
  musicLibrary = "${osConfig.custom.shares.media.root}/music/library";

  database = "${config.xdg.dataHome}/beets/library.db";
  databaseBackup = "${musicDir}/beets.db.backup";

  # Routine maintenance: beet-manage. Ad-hoc: beet mbsync (-p to preview), beet fingerprint, beet scrub.
  # Docs: https://beets.readthedocs.io/en/stable/plugins/index.html
  plugins = let
    providers = [ "musicbrainz" "chroma" "spotify" "deezer" ];
    # mbsubmit adds "Print tracks"/Picard choices when an import finds no MusicBrainz match.
    # After the fact: `beet mbsubmit <query>` prints a listing for MusicBrainz's track parser.
    metadata  = [ "fetchart" "embedart" "lyrics" "mbsync" "mbsubmit" "replaygain" ]; # lastgenre
    health    = [ "duplicates" "badfiles" "unimported" ];
    utility   = [ "edit" "playlist" "smartplaylist" "scrub" "fish" ];
  in providers ++ health ++ metadata ++ utility;
  basePackage = pkgs.python3.pkgs.beets.override {
    # Reference: https://github.com/NixOS/nixpkgs/blob/master/pkgs/tools/audio/beets/builtin-plugins.nix
    pluginOverrides = foldl' (acc: plugin: acc // { "${plugin}".enable = true; }) { } plugins;
  };

  # Sanity check + backup database file to NAS. Can't store the DB file in the NAS as it leads to lock issues.
  finalPackage = pkgs.writeShellApplication {
    name = "beet";
    runtimeInputs = [ pkgs.coreutils ];
    text = ''
      if [ ! -d "${musicLibrary}" ]; then
        echo "${musicLibrary} does not exist!"
        exit 1
      fi

      if [ ! -f "${database}" ]; then
        mkdir -p "$(dirname "${database}")"
        cp -f "${databaseBackup}" "${database}"
      fi

      status=0
      ${lib.getExe basePackage} "$@" || status=$?
      if [ "$status" -eq 0 ] && [ -f "${database}" ] && { [ ! -f "${databaseBackup}" ] || ! cmp -s "${database}" "${databaseBackup}"; }; then
        echo "Backing up beets library: ${database}"
        cp -f "${database}" "${databaseBackup}"
      fi
      exit "$status"
    '';
  };

  # Curated maintenance pass (custom, unlike the plain `beet` wrapper). mbsync stays manual: it rewrites tags library-wide (preview with -p).
  beet-manage = pkgs.writeShellApplication {
    name = "beet-manage";
    runtimeInputs = [ finalPackage pkgs.flac pkgs.mp3val ]; # flac/mp3val: external checkers `beet bad` shells out to
    text = ''
      beet update       # reconcile DB with on-disk moves/edits
      beet fetchart     # fetch missing covers (cautious)
      beet embedart     # embed covers into files
      beet lyrics       # fetch missing (synced) lyrics
      beet replaygain -a # album-gain analysis; skips files already tagged
      beet splupdate    # regenerate smart playlists
      beet bad          # report unplayable files
      beet duplicates   # report duplicate items
      beet unimported   # report files on disk beets isn't tracking
    '';
  };
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  home.packages = [ beet-manage ];
  programs.beets = {
    enable = true;
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
        playlist_dir = "${osConfig.custom.shares.media.root}/music/playlists";
      };
      fetchart = {
        auto = true;
        cautious = true;
      };
      lyrics.synced = true;
      replaygain.backend = "ffmpeg"; # default `command` backend (mp3gain) covers fewer formats than this library uses
      smartplaylist = {
        relative_to = musicLibrary;
        playlist_dir = "${osConfig.custom.shares.media.root}/music/playlists";
        # Generated files land beside the hand-written radio-*.m3u, so keep names distinct.
        playlists = [
          { name = "1990s.m3u"; query = "year:1990..1999"; }
          { name = "2000s.m3u"; query = "year:2000..2009"; }
        ];
      };
      musicbrainz = {
        extra_tags = ["catalognum" "country" "label" "media" "year"];
      };
    };
  };
}