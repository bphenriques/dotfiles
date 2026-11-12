#shellcheck shell=bash

# Whether dunst's notifications are paused, then again on every change it signals.
# dbus-monitor prints the property name and its value on consecutive lines.
dunstctl is-paused
next=0
dbus-monitor --session "type='signal',path='/org/freedesktop/Notifications',interface='org.freedesktop.DBus.Properties',member='PropertiesChanged'" | while read -r line; do
  if [[ $next == 1 ]]; then if [[ $line == *"boolean true"* ]]; then echo true; else echo false; fi; fi
  if [[ $line == *'string "paused"'* ]]; then next=1; else next=0; fi
done
