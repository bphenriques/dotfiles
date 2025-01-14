#shellcheck shell=bash
# udev rule that hotplugs display drivers.
#
# As documented by the ddcci-driver, we need to reload it everytime a device is (dis)connected.
# Then, we manually add/remove the device so that it gets exposed under `/sys/class/backlight/`
#
# Installation: tied to udev using systemd (https://wiki.archlinux.org/title/Udev#Spawning_long-running_processes)

fatal() { printf '%s\n' "$1" 1>&2; exit 1; }
backlight_compatible() { ddcutil getvcp 10 --bus="$1"; }
add_ddcci_device() { echo "Registering i2c device '$1'" && echo ddcci 0x37 | tee "/sys/bus/i2c/devices/$1/new_device"; }
list_external_i2c_devices() { ddcutil detect | grep -A1 'Display [0-9]' | grep -oP '/dev/.*$' | sed 's|/dev/i2c-||g'; }
register_backlight_devices() {
  for i2c_bus in $(list_external_i2c_devices); do
    if backlight_compatible "${i2c_bus}"; then
      add_ddcci_device "/i2c-${i2c_bus}"
    else
      echo "$i2c_bus does not support backlight according to ddcutil"
    fi
  done
}

case "${1:-}" in
  init)                       modprobe -r ddcci && modprobe ddcci && register_backlight_devices ;;
  list)                       list_external_i2c_devices                                         ;;
  register-backlight-devices) register_backlight_devices                                        ;;
esac

# TODO: maybe I _should_ make this smart and give another name to the backlight device so that it matches the desktop manager output (e.g., model).