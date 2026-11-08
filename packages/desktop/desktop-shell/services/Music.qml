pragma Singleton

import QtQuick
import Quickshell
import Quickshell.Services.Mpris

// Local mpd as published over MPRIS by mpdris2. Playback position is left out: MPRIS only updates it when polled.
Singleton {
  id: root

  readonly property var player: Mpris.players.values.find(player => player.identity === "Music Player Daemon") ?? null
  readonly property bool present: player !== null
  readonly property bool playing: player?.isPlaying ?? false
  // Bauer Media radio sends an XML document as the stream title; its song and artist are inside.
  readonly property string rawTitle: player?.trackTitle ?? ""
  readonly property bool xmlTitle: rawTitle.startsWith("<?xml")
  readonly property string title: xmlTitle ? field("DB_SONG_NAME") : rawTitle
  readonly property string artist: xmlTitle ? field("DB_LEAD_ARTIST_NAME") : player?.trackArtist ?? ""
  readonly property bool repeat: (player?.loopState ?? MprisLoopState.None) !== MprisLoopState.None
  readonly property bool shuffle: player?.shuffle ?? false

  function field(name) {
    return root.rawTitle.match(new RegExp("<" + name + ">([^<]*)</" + name + ">"))?.[1]?.trim() ?? "";
  }
}
