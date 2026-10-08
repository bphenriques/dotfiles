{ pkgs, lib, osConfig, config, ... }:
let

  musicDir = "${osConfig.fleet.shares.media.root}/music";
  musicLibrary = "${osConfig.fleet.shares.media.root}/music/library";
  mp3Library = "${osConfig.fleet.shares.media.root}/music/library-mp3";

  database = "${config.xdg.dataHome}/beets/library.db";
  databaseBackup = "${musicDir}/beets.db.backup";

  # Routine maintenance: beet-manage. Ad-hoc: beet mbsync (-p to preview), beet fingerprint, beet scrub.
  # Docs: https://beets.readthedocs.io/en/stable/plugins/index.html
  plugins = let
    providers = [ "musicbrainz" "chroma" "spotify" "deezer" ];
    metadata  = [ "fetchart" "embedart" "lyrics" "mbsync" "mbsubmit" "replaygain" ];
    health    = [ "duplicates" "badfiles" "unimported" ];
    utility   = [ "convert" "edit" "playlist" "smartplaylist" "scrub" "fish" ];
  in providers ++ health ++ metadata ++ utility;
  basePackage = pkgs.python3.pkgs.beets.override {
    # Reference: https://github.com/NixOS/nixpkgs/blob/master/pkgs/tools/audio/beets/builtin-plugins.nix
    pluginOverrides = lib.genAttrs plugins (_: { enable = true; });
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

  # -ar caps hi-res to what these players accept, -xerror refuses corrupt input.
  flacToMp3 = pkgs.writeShellApplication {
    name = "flac-to-mp3";
    runtimeInputs = [ pkgs.ffmpeg ];
    text = ''ffmpeg -v error -xerror -i "$1" -vn -ar 44100 -codec:a libmp3lame -b:a 256k "$2"'';
  };

  # Some sources ship FLACs with no STREAMINFO MD5, which leaves `flac -t` unable to verify them and `beet bad` red.
  flacEnsureMd5 = pkgs.writeShellApplication {
    name = "flac-ensure-md5";
    runtimeInputs = [ pkgs.flac pkgs.coreutils pkgs.findutils ];
    text = ''
      find "$1" -name '*.flac' -print0 | while IFS= read -r -d "" f; do
        [ "$(metaflac --show-md5sum "$f")" = "00000000000000000000000000000000" ] || continue
        tmp="$(dirname "$f")/.md5fix-$(basename "$f")"
        # A file that will not re-encode is corrupt: leave it for `beet bad`, never abort the pass.
        if ! flac -f -s "$f" -o "$tmp" 2>/dev/null; then
          rm -f "$tmp"
          echo "cannot re-encode: $f" >&2
          continue
        fi
        # Re-encoding is lossless, so the fresh MD5 must equal the original decoded stream.
        if [ "$(metaflac --show-md5sum "$tmp")" = "$(flac -dcs --force-raw-format --endian=little --sign=signed "$f" | md5sum | cut -d' ' -f1)" ]; then
          mv -f "$tmp" "$f"
          echo "added MD5: $f"
        else
          rm -f "$tmp"
          echo "FAILED to add MD5: $f" >&2
        fi
      done
    '';
  };

  # Curated maintenance pass (custom, unlike the plain `beet` wrapper). mbsync stays manual: it rewrites tags library-wide (preview with -p).
  beet-manage = pkgs.writeShellApplication {
    name = "beet-manage";
    runtimeInputs = [ finalPackage flacEnsureMd5 ]; # `beet bad` gets flac/mp3val from the badfiles plugin's own wrapper
    text = ''
      beet update       # reconcile DB with on-disk moves/edits
      beet fetchart     # fetch missing covers (cautious)
      beet embedart     # embed covers into files
      beet lyrics       # fetch missing (synced) lyrics
      beet replaygain -a # album-gain analysis; skips files already tagged
      beet splupdate    # regenerate smart playlists
      flac-ensure-md5 "${musicLibrary}" # keep every file verifiable so `beet bad` stays green
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
        playlist_dir = "${osConfig.fleet.shares.media.root}/music/playlists";
      };
      fetchart = {
        auto = true;
        cautious = true;
        cover_format = "JPEG"; # the resizer keeps the source format, and a 1000px PNG embeds ~20x larger than its JPEG equivalent
      };
      embedart = {
        maxwidth = 1000;
        quality = 90;
      };
      badfiles.check_on_import = true;
      unimported.ignore_subdirectories = [ ".stfolder" ]; # syncthing's folder marker, not stray music
      lyrics.synced = true;
      replaygain.backend = "ffmpeg"; # default `command` backend (mp3gain) covers fewer formats than this library uses
      convert = {
        dest = mp3Library;
        format = "mp3";
        formats.mp3 = {
          command = "${lib.getExe flacToMp3} $source $dest";
          extension = "mp3";
        };
        never_convert_lossy_files = true;
        album_art_maxwidth = 500; # convert never passes embedart.quality, so width is the only lever; the panel is 720x480 anyway
      };
      smartplaylist = {
        relative_to = musicLibrary;
        playlist_dir = "${osConfig.fleet.shares.media.root}/music/playlists";
        playlists = [
          { name = "1970s.m3u"; query = "year:1970..1979"; }
          { name = "1980s.m3u"; query = "year:1980..1989"; }
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