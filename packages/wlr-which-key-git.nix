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
    rev = "a4a6b747fafb0a015006a2978827c588b40a4562";
    hash = "sha256-nijUyHA4hRXAScFVmw0at6dTsGLpxc7ueeryfU+50HQ=";
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