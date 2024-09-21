{
  lib,
  stdenvNoCC,
  fetchFromGitLab,
  coreutils,
  sddm,
  qtbase,
  qtsvg,
  qtquickcontrols2,
  qtgraphicaleffects,
}:
stdenvNoCC.mkDerivation rec {
  name = "sddm-eucalyptus-drop";
  version = "2.0.0";
  src = fetchFromGitLab {
    owner = "Matt.Jolly";
    repo = "sddm-eucalyptus-drop";
    rev = "v2.0.0";
    sha256 = "wq6V3UOHteT6CsHyc7+KqclRMgyDXjajcQrX/y+rkA0=";
  };

  propagatedUserEnvPkgs = [
    sddm
    qtbase
    qtsvg
    qtgraphicaleffects
    qtquickcontrols2
  ];

  nativeBuildInputs = [
    coreutils
  ];

  installPhase = ''
    runHook preInstall
    local installDir=$out/share/sddm/themes/${name}
    mkdir -p $installDir
    cp -aR -t $installDir Main.qml Assets Components metadata.desktop theme.conf Backgrounds
    runHook postInstall
  '';

  meta = {
    description = "Eucalyptus Drop theme for SDDM";
    platforms = ["x86_64-linux"];
  };
}

# https://github.com/NixOS/nixpkgs/issues/292761
# https://github.com/Zhaith-Izaliel/sddm-sugar-candy-nix/blob/master/nix/default.nix
# https://github.com/ava5627/nixfiles/blob/f48301e36307e706a10a0eef1c1c61f7a8f532f5/packages/eucalyptus-drop.nix#L12
# https://github.com/NixOS/nixpkgs/blob/master/lib/generators.nix#L227

