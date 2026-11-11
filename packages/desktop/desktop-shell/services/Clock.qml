pragma Singleton

import QtQuick
import Quickshell
import qs

// The time to the minute, and the locale the clock and calendar name days and months in (Config.clock).
Singleton {
  id: root

  readonly property date date: clock.date
  readonly property var locale: Config.clock.locale ? Qt.locale(Config.clock.locale) : Qt.locale()
  readonly property int firstDayOfWeek: Config.clock.firstDayOfWeek >= 0 ? Config.clock.firstDayOfWeek : locale.firstDayOfWeek
  // Config.clock.format, else the locale's long date and short time.
  readonly property string text: Config.clock.format ? format(date, Config.clock.format)
    : date.toLocaleDateString(locale, Locale.LongFormat) + " " + date.toLocaleTimeString(locale, Locale.ShortFormat)

  // strftime, as waybar's clock takes it: %a %A %b %B %d %m %y %Y %H %I %M %p %%, a - after % drops the zero padding (%-d),
  // and %o, the English ordinal suffix of the day (st, nd, rd, th).
  function format(date, pattern) {
    const pad = (value, flag) => flag === "-" ? String(value) : String(value).padStart(2, "0");
    return pattern.replace(/%(-?)([a-zA-Z%])/g, (all, flag, code) => {
      switch (code) {
      case "a": return locale.dayName(date.getDay(), Locale.ShortFormat);
      case "A": return locale.dayName(date.getDay(), Locale.LongFormat);
      case "b": return locale.monthName(date.getMonth(), Locale.ShortFormat);
      case "B": return locale.monthName(date.getMonth(), Locale.LongFormat);
      case "d": return pad(date.getDate(), flag);
      case "m": return pad(date.getMonth() + 1, flag);
      case "y": return pad(date.getFullYear() % 100, flag);
      case "Y": return String(date.getFullYear());
      case "H": return pad(date.getHours(), flag);
      case "I": return pad(date.getHours() % 12 || 12, flag);
      case "M": return pad(date.getMinutes(), flag);
      case "p": return date.getHours() < 12 ? locale.amText : locale.pmText;
      case "o": return ordinal(date.getDate());
      case "%": return "%";
      default: return all;
      }
    });
  }

  function ordinal(day) {
    if (day >= 11 && day <= 13) return "th";
    return ["th", "st", "nd", "rd"][day % 10] ?? "th";
  }

  SystemClock {
    id: clock
    precision: SystemClock.Minutes
  }
}
