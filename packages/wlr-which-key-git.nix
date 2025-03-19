{
  lib,
  rustPlatform,
  fetchFromGitHub,
  pkg-config,
  cairo,
  glib,
  libxkbcommon,
  pango,
}:

# Remove on next release when it supports list rather than maps in the config
rustPlatform.buildRustPackage rec {
  pname = "wlr-which-key";
  version = "1.1.0";  # pretend

  src = fetchFromGitHub {
    owner = "MaxVerevkin";
    repo = "wlr-which-key";
    rev = "131d280cadf498d599c03a98ea0ade1ffc92f382";
    hash = "sha256-FGqzHrO8rU25XDLCW7RGHsFD7j61gxo6TaPJYHiwCdY=";
  };

  cargoHash = "sha256-eyNZ+ATw6W71KWLSQ+5PXSAwBY8OijUEn8DALx6U0ME=";

  nativeBuildInputs = [
    pkg-config
  ];

  buildInputs = [
    cairo
    glib
    libxkbcommon
    pango
  ];

  meta = with lib; {
    description = "Keymap manager for wlroots-based compositors";
    homepage = "https://github.com/MaxVerevkin/wlr-which-key";
    license = licenses.gpl3Only;
    maintainers = with maintainers; [ xlambein ];
    platforms = platforms.linux;
    mainProgram = "wlr-which-key";
  };
}