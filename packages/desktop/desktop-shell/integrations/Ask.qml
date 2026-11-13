pragma ComponentBehavior: Bound

import QtQuick
import QtQuick.Layouts
import Quickshell
import Quickshell.Io
import Quickshell.Wayland
import qs
import qs.ui

// Quick turns with an agent, in a panel in the middle of the screen; the conversation moves to a terminal from there.
// settings.ask: given a prompt, the session ("" for a new one) and an image ("" for none), prints a JSON object per line:
// {"run", "session"} first, {"status"} as it moves on (the tool it uses), {"approval"} (what it asks to run) when it waits
// for one, then {"session", "reply"} or {"error"}.
// settings.approve: given the run and allow or deny, answers its approval. settings.stop: given the run, stops it.
// settings.notify: given a title and a text, tells of a reply or an approval while hidden; prints `default` when clicked.
// settings.pasteImage: given a file, writes the clipboard's image there, failing when it holds none; `ask` then gets it.
// settings.open: given the session, opens it in a terminal. settings.title, settings.glyph: the header.
Integration {
  id: root

  // In lines, like the menu's width in pixels: declared, so the panel holds still (rule 4).
  readonly property int lines: 14
  readonly property string title: settings.title ?? name

  // [{ prompt, image, reply, error, stopped }], the last one waiting while busy.
  property var exchanges: []
  property string session: ""
  property string draft: ""
  // An image pasted for the next prompt, as a file under $XDG_RUNTIME_DIR.
  property string image: ""
  property bool shown: false
  property bool busy: false
  property bool unread: false
  // Bumped when a conversation ends, so output for an ended one is dropped.
  property int conversation: 0
  // The current run as the agent names it, what it waits to be allowed to run, and what it is doing.
  property string run: ""
  property string approval: ""
  property string activity: ""
  // Seconds the current run has taken, ticking only while it runs and the panel shows.
  property int sentAt: 0
  property int now: 0
  readonly property int waited: Math.max(0, now - sentAt)

  // Escape steps back without losing the conversation, as in a chat app; twice quickly hides.
  readonly property string escapeAction: approval ? "deny" : busy ? "stop" : draft || image ? "clear" : "hide"
  readonly property bool canMove: !busy && exchanges.length > 0
  property real lastEscape: 0

  // For the prompt, which lives in the panel's window: put a failed prompt back (Return retries it), or clear it.
  signal restore(string prompt)
  signal clearPrompt
  // Ctrl+V found no image, so it pastes text as usual.
  signal pasteText

  glyph: settings.glyph ?? ""
  active: busy || unread
  shortcut: settings.shortcut ?? ""

  function activate() {
    toggle();
  }

  function toggle() {
    shown = !shown;
    if (shown) unread = false;
  }

  function send(prompt) {
    prompt = prompt.trim();
    if ((!prompt && !image) || busy || request.stopping || !settings.ask) return false;
    // Asking again what failed or was stopped replaces that exchange, so the thread keeps one of each.
    const last = exchanges[exchanges.length - 1];
    const kept = last && last.error && last.prompt === prompt && last.image === image ? exchanges.slice(0, -1) : exchanges;
    exchanges = kept.concat([{ prompt: prompt, image: image, reply: "", error: "", stopped: false }]);
    const sent = image;
    image = "";
    busy = true;
    run = "";
    approval = "";
    activity = "";
    sentAt = now = Math.floor(Date.now() / 1000);
    request.conversation = conversation;
    request.command = commandLine(settings.ask, prompt, session, sent);
    request.running = true;
    return true;
  }

  function receive(line) {
    let answer;
    try {
      answer = JSON.parse(line);
    } catch (error) {
      return;
    }
    // Stopped before it said its name: closing the stream does not stop a run, so stop it now that it has one.
    if (request.stopping) {
      if (answer.run) {
        exec(settings.stop, answer.run);
        request.stopping = false;
        request.running = false;
      }
      return;
    }
    if (request.conversation !== conversation || !busy) return;
    if (answer.run) {
      run = answer.run;
      // Known from the start, so asking again after a stop carries on in the same session.
      session = answer.session ?? session;
    } else if (answer.approval) {
      approval = answer.approval;
      notify("Allow " + approval + "?");
    } else if (answer.status) {
      activity = answer.status;
    } else {
      finish(answer);
    }
  }

  function finish(answer) {
    if (answer.session) session = answer.session;
    const last = Object.assign({}, exchanges[exchanges.length - 1], { reply: answer.reply ?? "", error: answer.reply ? "" : answer.error ?? "No answer", stopped: !!answer.stopped });
    exchanges = exchanges.slice(0, -1).concat([last]);
    busy = false;
    approval = "";
    notify(last.error || last.reply);
    if (!shown) unread = true;
    if (last.error && draft === "" && !image) {
      draft = last.prompt;
      image = last.image;
      restore(last.prompt);
    }
  }

  function notify(text) {
    if (shown || !settings.notify) return;
    notifier.running = false;
    notifier.command = commandLine(settings.notify, title, text.split("\n")[0]);
    notifier.running = true;
  }

  // Ctrl+V: the clipboard's image, attached to the next prompt; without one, the text paste it would have been.
  function pasteImage() {
    if (!settings.pasteImage) return false;
    paster.path = Quickshell.env("XDG_RUNTIME_DIR") + "/" + name + "-" + Date.now();
    paster.command = commandLine(settings.pasteImage, paster.path);
    paster.running = true;
    return true;
  }

  function answerApproval(allowed) {
    if (!approval || !settings.approve) return;
    exec(settings.approve, run, allowed ? "allow" : "deny");
    approval = "";
  }

  // Stops the run where it runs, not only here; its prompt goes back to the input. Before the run has a name, the stream
  // stays open until it does (receive).
  function stop() {
    if (!busy) return;
    finish({ error: "Stopped", stopped: true });
    if (!settings.stop) {
      request.running = false;
    } else if (run) {
      exec(settings.stop, run);
      request.running = false;
    } else {
      request.stopping = true;
    }
  }

  // A new conversation; the unsent prompt stays.
  function reset() {
    stop();
    conversation++;
    exchanges = [];
    session = "";
    unread = false;
  }

  // Only between runs: a run lives as long as its stream here.
  function moveToTerminal() {
    if (!canMove) return;
    exec(settings.open, session);
    reset();
    shown = false;
  }

  // The panel's keys, from the keyboard or a click on the legend; whether a key was the panel's, so the rest reach the prompt.
  function act(key) {
    switch (key) {
    case "Escape": stepBack(); return true;
    case "Ctrl+y": answerApproval(true); return true;
    case "Ctrl+l": reset(); return true;
    case "Ctrl+o": moveToTerminal(); return true;
    case "Ctrl+v": return pasteImage();
    }
    return false;
  }

  function stepBack() {
    const twice = Date.now() - lastEscape < 400;
    lastEscape = Date.now();
    if (twice) {
      shown = false;
      return;
    }
    switch (escapeAction) {
    case "deny": answerApproval(false); break;
    case "stop": stop(); break;
    case "clear":
      clearPrompt();
      image = "";
      break;
    default: shown = false;
    }
  }

  // The web links in a reply, each once, without the punctuation that ends a sentence.
  function links(text) {
    const found = (text.match(/https?:\/\/[^\s<>"'()\[\]]+/g) ?? []).map(url => url.replace(/[.,;:!?]+$/, ""));
    return [...new Set(found)];
  }

  // A link as read: its host, then the last part of its path (recipes.example.com › bacalhau-com-natas).
  function linkLabel(url) {
    const [, host, path] = url.match(/^https?:\/\/([^\/?#]+)([^?#]*)/) ?? [null, url, ""];
    const last = path.split("/").filter(part => part !== "").pop();
    if (!last) return host;
    let page = last;
    try {
      page = decodeURIComponent(last);
    } catch (error) {}
    return host + " › " + page.replace(/\.html?$/, "");
  }

  // A link under a reply. Opened by the desktop's handler for links, the one thing here not from the config (README rule 2);
  // the panel hides so the browser shows.
  component LinkRow: ListItem {
    id: link
    required property string modelData
    Layout.fillWidth: true
    onActivated: {
      Qt.openUrlExternally(link.modelData);
      root.shown = false;
    }

    Icon {
      tone: "primary"
      glyph: Glyphs.link
      size: Metrics.line
    }

    Label {
      Layout.fillWidth: true
      tone: "primary"
      elide: Text.ElideMiddle
      text: root.linkLabel(link.modelData)
    }
  }

  // An image sent or about to be, four lines tall and as wide as its shape, so a screenshot gets no bands.
  component Thumbnail: Image {
    property string path
    visible: path !== ""
    source: path ? "file://" + path : ""
    fillMode: Image.PreserveAspectFit
    sourceSize.height: Metrics.line * 4
    Layout.preferredHeight: Metrics.line * 4
    Layout.preferredWidth: implicitHeight > 0 ? Layout.preferredHeight * implicitWidth / implicitHeight : 0
  }

  // A prompt and what came of it: the reply (selectable, copied as selected), its links, or the wait for it.
  component Exchange: ColumnLayout {
    id: exchange
    required property var modelData
    required property int index
    readonly property bool waiting: root.busy && index === root.exchanges.length - 1
    readonly property real indent: Metrics.line + Config.theme.spacing
    Layout.fillWidth: true
    spacing: Config.theme.spacing / 2

    RowLayout {
      Layout.fillWidth: true
      spacing: Config.theme.spacing

      Icon {
        Layout.alignment: Qt.AlignTop
        tone: "dim"
        glyph: Glyphs.submenu
        size: Metrics.line
      }

      Label {
        Layout.fillWidth: true
        tone: "dim"
        wrapMode: Text.Wrap
        text: exchange.modelData.prompt
      }
    }

    Thumbnail {
      Layout.leftMargin: exchange.indent
      path: exchange.modelData.image ?? ""
    }

    TextField {
      Layout.fillWidth: true
      Layout.leftMargin: exchange.indent
      visible: text !== ""
      // A stop was asked for, so it only reports; a failure needs attention.
      tone: exchange.modelData.stopped ? "dim" : exchange.modelData.error ? "warning" : "normal"
      // A line that is only a link shows once, as its row below.
      text: exchange.modelData.error || exchange.modelData.reply.split("\n").filter(line => !/^\s*https?:\/\/\S+\s*$/.test(line)).join("\n").trim()
      readOnly: true
      selectByMouse: true
      persistentSelection: true
      activeFocusOnPress: false
      copyOnSelect: true
    }

    Repeater {
      model: root.links(exchange.modelData.reply)

      LinkRow {
        Layout.leftMargin: exchange.indent - Config.theme.itemPaddingX
      }
    }

    // An approval needs attention; the rest only reports: the tool in use, and the time once the wait is noticeable.
    RowLayout {
      visible: exchange.waiting
      Layout.leftMargin: exchange.indent
      spacing: Config.theme.spacing

      Spinner {
        tone: "dim"
        size: Metrics.line
        mood: root.approval ? "attention" : "working"
      }

      Label {
        Layout.fillWidth: true
        tone: root.approval ? "warning" : "dim"
        wrapMode: Text.Wrap
        text: root.approval ? "Allow " + root.approval + "?" : root.activity ? root.activity + " · " + root.waited + "s" : root.waited >= 2 ? root.waited + "s" : ""
      }
    }
  }

  Timer {
    interval: 1000
    repeat: true
    running: root.busy && root.shown
    onTriggered: root.now = Math.floor(Date.now() / 1000)
  }

  Process {
    id: request
    property int conversation: 0
    // Stopped before its run had a name: kept open until it has one.
    property bool stopping: false

    stdout: SplitParser {
      onRead: line => root.receive(line)
    }

    // Ended without an answer: it crashed or was killed.
    onExited: {
      stopping = false;
      if (root.busy && conversation === root.conversation) root.finish({ error: "No answer" });
    }
  }

  Process {
    id: paster
    property string path

    onExited: code => {
      if (code === 0) root.image = path;
      else root.pasteText();
    }
  }

  Process {
    id: notifier

    stdout: SplitParser {
      onRead: line => {
        if (line.trim() !== "default") return;
        root.shown = true;
        root.unread = false;
      }
    }
  }

  IpcHandler {
    target: root.name

    function toggle(): void {
      root.toggle();
    }
  }

  LazyLoader {
    active: root.shown

    OverlayWindow {
      anchors.top: true
      anchors.bottom: true
      anchors.left: true
      anchors.right: true
      WlrLayershell.namespace: root.name
      WlrLayershell.keyboardFocus: WlrKeyboardFocus.Exclusive
      BackgroundEffect.blurRegion: Region {
        item: card
        radius: Config.theme.radius
      }

      // A click beside the panel hides it, the conversation kept.
      MouseArea {
        anchors.fill: parent
        onClicked: root.shown = false
      }

      Shadow {
        anchors.fill: card
        radius: card.radius
      }

      Surface {
        id: card
        anchors.centerIn: parent
        width: Config.menu.width
        height: content.implicitHeight + 2 * Config.theme.padding

        MouseArea {
          anchors.fill: parent
        }

        ColumnLayout {
          id: content
          anchors.verticalCenter: parent.verticalCenter
          anchors.left: parent.left
          anchors.right: parent.right
          anchors.margins: Config.theme.padding
          spacing: Config.theme.padding

          RowLayout {
            Layout.fillWidth: true
            Layout.leftMargin: Config.theme.itemPaddingX
            Layout.rightMargin: Config.theme.itemPaddingX
            spacing: Config.theme.spacing

            Icon {
              glyph: root.glyph
              size: Metrics.line
            }

            Label {
              Layout.fillWidth: true
              font.bold: true
              text: root.title
            }
          }

          // Only the keys no one would guess (Return sends, as in any chat), each while it applies.
          KeyLegend {
            keys: [{ key: "Shift+Return", what: "newline" }]
              .concat(root.approval ? [{ key: "Ctrl+y", what: "allow", clickable: true }] : [])
              .concat(root.exchanges.length > 0 ? [{ key: "Ctrl+l", what: "new", clickable: true }] : [])
              .concat(root.canMove ? [{ key: "Ctrl+o", what: "Move to terminal...", clickable: true }] : [])
              // Last, so its changing word moves nothing; twice quickly hides (README).
              .concat([{ key: "Escape", what: root.escapeAction, clickable: true }])
            onPicked: key => root.act(key)
          }

          Divider {
            Layout.fillWidth: true
          }

          ColumnLayout {
            Layout.fillWidth: true
            Layout.preferredHeight: root.lines * Metrics.line
            Layout.leftMargin: Config.theme.itemPaddingX
            Layout.rightMargin: Config.theme.itemPaddingX
            // History and prompt as far apart as two exchanges.
            spacing: Config.theme.padding

            // The conversation, kept scrolled to its end.
            Flickable {
              id: thread
              Layout.fillWidth: true
              Layout.fillHeight: true
              clip: true
              contentHeight: exchangeList.implicitHeight
              boundsBehavior: Flickable.StopAtBounds
              onContentHeightChanged: contentY = Math.max(0, contentHeight - height)
              onHeightChanged: contentY = Math.max(0, contentHeight - height)

              function page(down) {
                contentY = Math.max(0, Math.min(contentHeight - height, contentY + (down ? 1 : -1) * height * 0.8));
              }

              ColumnLayout {
                id: exchangeList
                width: thread.width
                // Wider between exchanges than within one, so each reads as a unit.
                spacing: Config.theme.padding

                Repeater {
                  model: root.exchanges

                  Exchange {}
                }
              }
            }

            // Pasted for the next prompt; Escape clears it with the text.
            Thumbnail {
              Layout.leftMargin: Metrics.line + Config.theme.spacing
              path: root.image
            }

            RowLayout {
              Layout.fillWidth: true
              spacing: Config.theme.spacing

              Icon {
                Layout.alignment: Qt.AlignTop
                tone: "primary"
                glyph: Glyphs.submenu
                size: Metrics.line
              }

              TextArea {
                id: prompt
                Layout.fillWidth: true
                Layout.preferredHeight: implicitHeight
                placeholder: "Ask " + root.title
                onTextChanged: root.draft = text
                Component.onCompleted: set(root.draft)

                // Return sends and Shift+Return breaks the line; the rest go to root.act, as the legend's clicks do.
                onKeyPressed: event => {
                  const plain = !(event.modifiers & (Qt.ShiftModifier | Qt.ControlModifier | Qt.AltModifier));
                  if ((event.key === Qt.Key_Return || event.key === Qt.Key_Enter) && plain) {
                    if (root.send(text)) clear();
                  } else if (event.key === Qt.Key_PageUp || event.key === Qt.Key_PageDown) {
                    thread.page(event.key === Qt.Key_PageDown);
                  } else if (!root.act(MenuKeys.name(event))) {
                    return;
                  }
                  event.accepted = true;
                }

                Connections {
                  target: root

                  function onRestore(text) {
                    if (prompt.text === "") prompt.set(text);
                  }

                  function onClearPrompt() {
                    prompt.clear();
                  }

                  function onPasteText() {
                    prompt.paste();
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
