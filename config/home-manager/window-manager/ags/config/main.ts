import { Bar } from "./widget/bar/bar.js"
import { NotificationPopups } from "./widget/notification-popup.js"
import { setupNotificationDaemon } from "./notification-daemon/init.js"


// https://user-images.githubusercontent.com/36706276/192147190-cf9cf4df-94cb-4a3b-b9d8-137ed0c2538f.png
// https://github.com/fufexan/dotfiles/blob/main/home/services/ags/windows/bar/modules/bluetooth.js

// https://aylur.github.io/ags-docs/config/theming/
function debug() {
  Utils.monitorFile(
      // directory that contains the scss files
      `${App.configDir}/style`,

      // reload function
      function() {
          // main scss file
          //const scss = `${App.configDir}/style.scss`

          // target css file
          //const css = `/tmp/my-style.css`
          console.log("reloading")
          const css = `${App.configDir}/style/style.css`
          // compile, reset, apply
          //Utils.exec(`sassc ${scss} ${css}`)
          App.resetCss()
          App.applyCss(css)
      },
  )
}

/*
function addWindows(windows) {
  windows.forEach(win => App.addWindow(win));
}

 Utils.idle(async () => {
  addWindows([
    IndicatorWidget(),
    Quicksettings(),
    await Launcher(),
    PowerMenu(),
    PopupNotifications(),
  ]);

  const display = Gdk.Display.get_default();
  for (let m = 0;  m < display?.get_n_monitors();  m++) {
    const monitor = display?.get_monitor(m);
    addMonitorWindows(monitor);
  }

  display?.connect("monitor-added", (disp, monitor) => {
    addMonitorWindows(monitor);
  });

  display?.connect("monitor-removed", (disp, monitor) => {
    App.windows.forEach(win => {
      if(win.gdkmonitor === monitor) App.removeWindow(win);
    });
  });


}); */

App.addIcons(`${App.configDir}/assets`)
App.config({
    onConfigParsed: () => {
      debug()
      setupNotificationDaemon()
    },
    style: `${App.configDir}/style/style.css`,
    windows: [
      NotificationPopups(),
      Bar(0)
    ]
})