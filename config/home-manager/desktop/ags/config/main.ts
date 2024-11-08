import { Bar } from "./widget/bar/bar.js"
import { NotificationPopups } from "./widget/notification-popup.js"
import { setupNotificationDaemon } from "./notification-daemon/init.js"

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

debug()
App.addIcons(`${App.configDir}/assets`)
App.config({
    onConfigParsed: () => {
      setupNotificationDaemon()
    },
    style: `${App.configDir}/style/style.css`,
    windows: [
      NotificationPopups(),
      Bar(0)
    ]
})