{
  lib,
  pkgs,
  homepageUrl ? "",
  ...
}:
let
  emailTemplateMd = pkgs.writeTextFile {
    name = "wg-manage-email-template.md";
    text = builtins.replaceStrings [ "{{HOMEPAGE_URL}}" ] [ homepageUrl ] (lib.fileContents ./email-template.md);
  };

  # Convert Markdown → HTML at build time (pandoc not needed at runtime)
  emailTemplate = pkgs.runCommand "wg-manage-email-template.html" {
    nativeBuildInputs = [ pkgs.pandoc ];
  } ''pandoc -f markdown -t html --standalone "${emailTemplateMd}" -o "$out"'';

  script = pkgs.writeTextFile {
    name = "wg-manage.nu";
    text = lib.fileContents ./script.nu;
  };
in
pkgs.writeShellApplication {
  name = "wg-manage-bin";
  runtimeInputs = with pkgs; [ nushell wireguard-tools qrencode coreutils mutt ];
  text = ''
    export WG_EMAIL_TEMPLATE_FILE="${emailTemplate}"
    exec nu ${script} "$@"
  '';
  meta.platforms = lib.platforms.linux;
}
