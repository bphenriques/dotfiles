{ config, lib, pkgs, ... }:
let
  cfg = config.programs.quick-key;
  makoctl = lib.getExe' config.services.mako.package "makoctl";
  jq = lib.getExe pkgs.jq;
  colors = config.lib.stylix.colors.withHashtag;

  # Mod+N: the actions of the newest notification that has any, in fuzzel, its summary as the prompt.
  # A card without actions (a song) never takes it from a screenshot; makoctl menu fails on one anyway.
  notificationActions = pkgs.writeShellScript "notification-actions" ''
    IFS=$'\t' read -r id count summary < <(${makoctl} list -j | ${jq} -r 'first(.[] | select(.actions | length > 0)) | [.id, (.actions | length), .summary] | @tsv')
    [ -n "$id" ] || exit 0
    exec ${makoctl} menu -n "$id" -- ${lib.getExe config.programs.fuzzel.package} --dmenu --prompt "$summary  " --lines "$count"
  '';
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  # quick-key's cards in look. Senders own their actions (notify-send --action --wait).
  services.mako = {
    enable = true;
    settings = {
      font = "${config.stylix.fonts.monospace.name} ${toString cfg.settings.theme.fontSize}";
      background-color = colors.base00 + "eb";
      text-color = colors.base05;
      border-color = colors.base03;
      progress-color = "over ${colors.base0D}40";
      border-size = 1;
      border-radius = 10;
      padding = 14;
      icon-location = "right";
      max-icon-size = 56;
      icon-border-radius = 6;
      width = 524;
      height = 120;
      # Top right, under quick-key's corner pills (recording, dictation); the menu sits at the bottom.
      anchor = "top-right";
      outer-margin = "78,35,0,0";
      margin = "0,0,8,0";
      max-visible = 3;
      max-history = 10;
      default-timeout = 8000;
      # The same app, summary and body again counts on the card instead of adding one.
      group-by = "app-name,summary,body";
    };
    # In order: later criteria win, which settings (sorted by name) would not keep.
    extraConfig = ''
      [urgency=low]
      default-timeout=3000

      [urgency=critical]
      default-timeout=0
      border-color=${colors.base08}

      [grouped]
      format=<b>%s</b>  ×%g\n%b

      [hidden]
      format=+%h

      # The screenshot script sends its own, with actions.
      [app-name=niri summary="Screenshot captured"]
      invisible=1
      on-notify=exec ${makoctl} dismiss -n "$id" --no-history

      # Waits for you.
      [app-name=Alfredo]
      default-timeout=0

      # Mod+D: only critical ones and Alfredo show through.
      [mode=do-not-disturb]
      invisible=1

      [mode=do-not-disturb urgency=critical]
      invisible=0

      [mode=do-not-disturb app-name=Alfredo]
      invisible=0
    '';
  };

  # The do-not-disturb bell in the bar, and in the pop-up as it turns.
  programs.quick-key.extraPackages = [ config.services.mako.package ];
  programs.quick-key.settings = {
    plugins.mako = { };
    popups.show = [ "mako" ];
  };
  my.quick-key.bar.mako = [ "mako" ];

  my.programs.niri = {
    bindings = {
      "Mod+N"      = ''spawn "${notificationActions}"'';
      "Mod+Ctrl+N" = ''spawn "${makoctl}" "dismiss"'';
      "Mod+D"      = ''spawn "${makoctl}" "mode" "-t" "do-not-disturb"'';
    };
    # Off screencasts and screenshots.
    layerRules.screenCapture.block = [ ''namespace="^notifications$"'' ];
  };
}
