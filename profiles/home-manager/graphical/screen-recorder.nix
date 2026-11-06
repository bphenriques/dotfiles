{ lib, pkgs, config, self, ... }:
let
  inherit (builtins) listToAttrs;
  inherit (lib) nameValuePair;

  mkIcon = self.lib.builders.mkNerdFontIcon { textColor = config.lib.stylix.colors.withHashtag.base07; };

  package = self.packages.screen-recorder.override {
    recordIcon = mkIcon "screen-recorder-record-icon" "󰑋";
    informationIcon = mkIcon "screen-recorder-information-icon" "";
    errorIcon = mkIcon "screen-recorder-error-icon" "";
  };
  screen-recorder = lib.getExe package;
  directory = config.xdg.userDirs.extraConfig.RECORDINGS;

  screenRecordingActions = [
    { id = "recording-screen-audio";    symbol = "󰹑"; label = "Screen (with audio)";  exec = ''${screen-recorder} screen-audio "${directory}"''; }
    { id = "recording-screen-no-audio"; symbol = "󰹑"; label = "Screen (no audio)";    exec = ''${screen-recorder} screen-no-audio "${directory}"''; }
    { id = "recording-region-audio";    symbol = ""; label = "Region (with audio)";  exec = ''${screen-recorder} region-audio "${directory}"''; }
    { id = "recording-region-no-audio"; symbol = ""; label = "Region (no audio)";    exec = ''${screen-recorder} region-no-audio "${directory}"''; }
    { id = "recording-stop";            symbol = ""; label = "Stop";                 exec = ''${screen-recorder} stop''; }
  ];

  dmenu = self.lib.builders.writeFuzzelDmenuApplication {
    name = "screen-recorder-menu";
    entries = lib.map (e: { inherit (e) exec; label = "${e.symbol}     ${e.label}"; }) screenRecordingActions;
    extraArgs = ''--minimal-lines --hide-prompt'';
  };
in
{
  home.packages = [
    pkgs.wl-screenrec
    package
    dmenu
    (pkgs.makeDesktopItem {
      name = "screen-recorder-menu";
      desktopName = "Screen Recording";
      icon = mkIcon "screen-recorder" "󰑋";
      exec = lib.getExe dmenu;
      actions = let
        toAction = b: nameValuePair b.id {
          name = b.label;
          icon = mkIcon b.id b.symbol;
          inherit (b) exec;
        };
      in listToAttrs (lib.map toAction screenRecordingActions);
    })
  ];
}
