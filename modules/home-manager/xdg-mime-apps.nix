{ lib, config, ... }:
let
  inherit (lib) foldl';

  cfg = config.custom.xdgDefaultApps;

  setDefault = types: target: foldl' (acc: type: acc // { "${type}" = target; }) { } types;
  mkDefaultAppOption = type: lib.mkOption {
    type = lib.types.listOf lib.types.str;
    description = lib.mdDoc ''Default app to open ${type} files'';
    default = [];
  };

  mkMimeOption = description: default: lib.mkOption {
    type = lib.types.listOf lib.types.str;
    description = lib.mdDoc ''List of mimes describing ${description} files'';
    default = [];
  };
in
{
  options.custom.xdgDefaultApps = {
    enable = lib.mkEnableOption "xdg-custom" // {
      default = config.xdg.mimeApps.enable;
    };

    mimes = {
      text = mkMimeOption "text" [ "text/plain" ];
      image = mkMimeOption "image" [
        "image/bmp"
        "image/gif"
        "image/jpeg"
        "image/jpg"
        "image/pjpeg"
        "image/png"
        "image/svg+xml"
        "image/tiff"
        "image/vnd.microsoft.icon"
        "image/webp"
        "image/x-bmp"
        "image/x-pcx"
        "image/x-png"
        "image/x-portable-anymap"
        "image/x-portable-bitmap"
        "image/x-portable-graymap"
        "image/x-portable-pixmap"
        "image/x-tga"
        "image/x-xbitmap"
      ];

      audio = mkMimeOption "audio" [
        "application/ogg"
        "application/vnd.apple.mpegurl"
        "application/x-ogg"
        "application/x-ogm-audio"
        "application/xspf+xml"
        "audio/aac"
        "audio/ac3"
        "audio/flac"
        "audio/m4a"
        "audio/mp3"
        "audio/mp4"
        "audio/mpeg"
        "audio/mpegurl"
        "audio/ogg"
        "audio/vnd.rn-realaudio"
        "audio/vorbis"
        "audio/x-aac"
        "audio/x-flac"
        "audio/x-m4a"
        "audio/x-mp3"
        "audio/x-mpeg"
        "audio/x-mpegurl"
        "audio/x-ms-wma"
        "audio/x-musepack"
        "audio/x-oggflac"
        "audio/x-opus+ogg"
        "audio/x-pn-realaudio"
        "audio/x-scpls"
        "audio/x-speex"
        "audio/x-vorbis"
        "audio/x-vorbis+ogg"
        "audio/x-wav"
        "x-content/audio-player"
      ];

      video = mkMimeOption "video" [
        "video/mp2t"
        "video/mp4"
        "video/mpeg"
        "video/ogg"
        "video/webm"
        "video/x-flv"
        "video/x-matroska"
        "video/x-msvideo"
      ];

      inode = mkMimeOption "inode" [
        "inode/file"
        "inode/directory"
        "inode/mount-point"
      ];

      archive = mkMimeOption "archive" [
        "application/zip"
        "application/rar"
        "application/7z"
        "application/*tar"
      ];

      document = mkMimeOption "document" [
        "application/vnd.comicbook-rar"
        "application/vnd.comicbook+zip"
        "application/x-cb7"
        "application/x-cbr"
        "application/x-cbt"
        "application/x-cbz"
        "application/x-ext-cb7"
        "application/x-ext-cbr"
        "application/x-ext-cbt"
        "application/x-ext-cbz"
        "application/x-ext-djv"
        "application/x-ext-djvu"
        "image/vnd.djvu+multipage"
        "application/x-bzdvi"
        "application/x-dvi"
        "application/x-ext-dvi"
        "application/x-gzdvi"
        "application/x-bzpdf"
        "application/x-ext-pdf"
        "application/x-gzpdf"
        "application/x-xzpdf"
        "application/postscript"
        "application/x-bzpostscript"
        "application/x-gzpostscript"
        "application/x-ext-eps"
        "application/x-ext-ps"
        "image/x-bzeps"
        "image/x-eps"
        "image/x-gzeps"
        "image/tiff"
        "application/oxps"
        "application/vnd.ms-xpsdocument"
        "application/illustrator"
      ];

      internet = mkMimeOption "internet" [
        "application/x-extension-htm"
        "application/x-extension-html"
        "application/x-extension-shtml"
        "application/x-extension-xht"
        "application/x-extension-xhtml"
        "application/xhtml+xml"
        "text/html"
        "x-scheme-handler/chrome"
        "x-scheme-handler/http"
        "x-scheme-handler/https"
        "text/xml"
        "x-scheme-handler/about"
        "x-scheme-handler/ftp"
        "x-scheme-handler/http"
        "x-scheme-handler/unknown"
        "x-scheme-handler/https"
      ];

      pdf = mkMimeOption "pdf" [ "application/pdf" ];
    };

    text = mkDefaultAppOption "text";
    image = mkDefaultAppOption "image";
    audio = mkDefaultAppOption "audio";
    video = mkDefaultAppOption "video";
    document = mkDefaultAppOption "document";
    internetBrowser = mkDefaultAppOption "internetBrowser";
    archive = mkDefaultAppOption "archive";
    pdf = (mkDefaultAppOption "pdf") // {
      default = cfg.document;
    };

    fileBrowser = mkDefaultAppOption "fileBrowser";
  };

  config = lib.mkIf cfg.enable {
    xdg.mimeApps.defaultApplications = lib.attrsets.mergeAttrsList [
      (setDefault cfg.mimes.text              cfg.text)
      (setDefault cfg.mimes.image             cfg.image)
      (setDefault cfg.mimes.audio             cfg.audio)
      (setDefault cfg.mimes.video             cfg.video)
      (setDefault cfg.mimes.document          cfg.document)
      (setDefault cfg.mimes.internet          cfg.internetBrowser)
      (setDefault cfg.mimes.archive           cfg.archive)
      (setDefault cfg.mimes.pdf               cfg.pdf)
      (setDefault cfg.mimes.inode             cfg.fileBrowser)
    ];
  };
}
