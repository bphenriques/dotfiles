{ config, lib, pkgs, ... }:
{
  programs.vscode = {
    enable = true;
    package = pkgs.vscodium;

    # Notes:
    # - Does not unistall extensions once removed.
    # - Update using the following: export temp=$(mktemp) && curl -s https://raw.githubusercontent.com/NixOS/nixpkgs/master/pkgs/misc/vscode-extensions/update_installed_exts.sh > $temp && chmod +x $temp && $temp $(whereis codium)
    extensions = with pkgs.vscode-extensions; [
      scalameta.metals    # Scala Language Server
      scala-lang.scala    # Scala Language
      bbenoist.Nix        # Nix
    ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
      {
        name = "markdown-all-in-one";
        publisher = "yzhang";
        version = "3.4.0";
        sha256 = "C5d2I0srdUGcmmvW2tRlMvD1RyFsUqECIQ0xLZ7ODkY=";
      }
      {
        name = "vscode-markdown-notes";
        publisher = "kortina";
        version = "0.0.24";
        sha256 = "0x433slvgnqislcrhdq9zy6fmznk0mqkqq4yjs4mbzrq1l40z4dg";
      }
      {
        name = "vscode-paste-image";
        publisher = "mushan";
        version = "1.0.4";
        sha256 = "1wkplvrn31vly5gw35hlgpjpxgq3dzb16hz64xcf77bwcqfnpakb";
      }
      {
        name = "foam-vscode";
        publisher = "foam";
        version = "0.13.3";
        sha256 = "0ih2a3xl4ki6r14nwx968sg1avz43jvs59z6ijm0l7xhgqv3dl8h";
      }
      {
        name = "spellright";
        publisher = "ban";
        version = "3.0.58";
        sha256 = "07891rad3261ciaql2s6adaxmd2sm5w6z4s8r5zfldiv4wrk2370";
      }
    ];

    userSettings = {
      "window.title" = "\${activeEditorLong}\${separator}\${rootName}"; # Let me use the free space!
      "explorer.confirmDragAndDrop" = false;                            # Don't bother me when I want to drag-and-drop files.
      "editor.tabSize" = 2;                                             # Because I prefer smaller tab sizes.
      "editor.formatOnPaste" = true;                                    # Format on paste to keep things tidier.
      "editor.formatOnSave" = true;                                     # Format on save to keep things tidier.
      "files.trimFinalNewlines" = true;                                 # Keep files trimmed.
      "files.insertFinalNewline" = true;                                # Because POSIX compliance (todo: read why is that really important, maybe to identify the last line?)
      "files.autoSave" = "onFocusChange";                               # Because I am lazy and I often source control the files.
      "workbench.editor.highlightModifiedTabs" = true;                  # Better highlight on modified tabs.
      "workbench.editor.enablePreview" = false;                         # I often prefer small files therefore I thank the extra space.
      "markdown.preview.scrollEditorWithPreview" = true;                # Syncronize editor with preview.
      "markdown.preview.scrollPreviewWithEditor" = true;                # Syncronize preview with editor.editor with preview.
    };
  };
}
