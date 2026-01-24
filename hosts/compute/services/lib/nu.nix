{ pkgs, lib }:
{
  # TODO Move somewhere else.
  checkedScript = name: src:
    pkgs.runCommand "${name}-checked" { } ''
      ${lib.getExe pkgs.nushell} --no-config-file -c 'nu-check "${src}"'
      cp ${src} $out
    '';
}
