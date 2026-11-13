# Voice dictation: Voxtype types what you say into the focused window as you speak.
{ config, lib, pkgs, self, ... }:
let
  voxtype = lib.getExe config.services.voxtype.package;
in
lib.mkIf pkgs.stdenv.hostPlatform.isLinux {
  services.voxtype = {
    enable = true;
    package = pkgs.voxtype-vulkan;
    loadModels = [ "large-v3-turbo" ];
    wayland.display = "wayland-1";   # niri's socket; the module puts wtype on the service's PATH only once one is set

    settings = {
      hotkey = {
        enabled = false;   # niri binds the keys
        mode = "toggle";   # streaming needs it
      };
      audio.max_duration_secs = 300;
      # Streaming re-transcribes up to 29s of audio every 0.8s: a discrete-class GPU, which the host selects.
      whisper = {
        model = "large-v3-turbo";
        language = "auto";
        streaming = true;
      };
      output = {
        mode = "type";
        fallback_to_clipboard = true;
        notification.on_transcription = false;   # the text itself is the feedback
      };
      osd.enabled = false;   # desktop-shell's dictation pill shows it
    };
  };

  my.programs.desktop-shell.integrations.dictation.settings = {
    glyph = self.lib.glyphs."fa-microphone_lines";
    status = "${voxtype} status --follow | ${lib.getExe pkgs.gnused} -u 's/^streaming$/recording/'";
    stop = "${voxtype} record stop";
  };

  my.programs.niri.bindings = {
    "Mod+V repeat=false" = ''spawn "${voxtype}" "record" "toggle"'';
    "Mod+Shift+V repeat=false" = ''spawn "${voxtype}" "record" "cancel"'';
  };
}
