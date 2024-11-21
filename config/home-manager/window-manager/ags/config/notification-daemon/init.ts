import battery from "./battery"

function exampleNotification() {
  Utils.timeout(100, () => Utils.notify({
      summary: "Notification Popup Example",
      iconName: "info-symbolic",
      body: "Lorem ipsum dolor sit amet, qui minim labore adipisicing "
          + "minim sint cillum sint consectetur cupidatat.",
      actions: {
          "Cool": () => print("pressed Cool"),
      },
  }))
}

export function setupNotificationDaemon() {
    try {
        battery()
        //exampleNotification()
    } catch (error) {
        logError(error)
    }
}