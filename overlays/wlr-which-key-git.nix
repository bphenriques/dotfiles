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
  version = "git";  # pretend

  src = fetchFromGitHub {
    owner = "MaxVerevkin";
    repo = "wlr-which-key";
    rev = "e2aa26ef0174d4acbfaf0b36f610b2aa326ceea3";
    hash = "sha256-4e+i/P5QjXsptkI43Co+4LBB5cJ0WbR/wn2geG5Yfz4=";
  };

  cargoHash = "sha256-ioCPuEbCmFDmzjo1AjLmrcjbhX/1ZvltDp9WDa9oM8w=";

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