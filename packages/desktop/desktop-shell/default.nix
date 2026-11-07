{ lib, stdenvNoCC }:
stdenvNoCC.mkDerivation {
  name = "desktop-shell";
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.unions [ ./shell.qml ./Config.qml ./services ./ui ./widgets ];
  };
  installPhase = ''
    mkdir -p $out/share/desktop-shell
    cp -r . $out/share/desktop-shell
  '';
  meta.platforms = lib.platforms.linux;
}
