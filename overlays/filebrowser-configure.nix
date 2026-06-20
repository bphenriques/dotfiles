# `filebrowser-configure`: provisions FileBrowser proxy-auth users + per-user scopes and
# rebuilds the DB from the declared config (drops removed users). Self-contained — bundles
# nushell + the filebrowser CLI. Consumers pass FILEBROWSER_CONFIG_FILE (JSON, see
# lib/builders.nix mkFilebrowserConfig), FILEBROWSER_DB and FILEBROWSER_ROOT via the env.
_final: prev:
let
  script = prev.runCommandLocal "filebrowser-configure.nu" { } ''
    ${prev.lib.getExe prev.nushell} --no-config-file --commands 'if not (nu-check "${./filebrowser-configure.nu}") { exit 1 }'
    cp ${./filebrowser-configure.nu} $out
  '';
in
{
  filebrowser-configure = prev.writeShellApplication {
    name = "filebrowser-configure";
    runtimeInputs = [ prev.nushell prev.filebrowser ];
    text = ''exec nu ${script} "$@"'';
  };
}
