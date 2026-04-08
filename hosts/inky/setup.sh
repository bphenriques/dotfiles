#!/usr/bin/env bash
# Idempotent setup for Raspberry Pi OS Lite on the inky host.
# Built via: nix build .#inky-setup
# Run as root on the Pi.
set -euo pipefail

HOSTNAME="inky"
USERNAME="bphenriques"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"
CONFIG_DIR="$SCRIPT_DIR/config"
MARKER_BEGIN="# BEGIN dotfiles-${HOSTNAME}"
MARKER_END="# END dotfiles-${HOSTNAME}"

info()    { printf '[ .. ] %s\n' "$1"; }
success() { printf '[ OK ] %s\n' "$1"; }
fatal()   { printf '[FAIL] %s\n' "$1" >&2; exit 1; }

[[ $(id -u) -eq 0 ]] || fatal "Must run as root"
[[ -d "$CONFIG_DIR" ]] || fatal "Config directory not found: $CONFIG_DIR"

# shellcheck source=config/generated/setup.env
source "$CONFIG_DIR/generated/setup.env"

# Replace or insert a managed block in a file.
replace_block() {
  local target="$1" fragment="$2"
  if grep -qF "$MARKER_BEGIN" "$target" 2>/dev/null; then
    sed -i "/$MARKER_BEGIN/,/$MARKER_END/d" "$target"
  fi
  {
    echo "$MARKER_BEGIN"
    cat "$fragment"
    echo "$MARKER_END"
  } >> "$target"
}

setup_packages() {
  export DEBIAN_FRONTEND=noninteractive

  # Kill unattended-upgrades to avoid dpkg lock conflicts
  systemctl stop unattended-upgrades.service 2>/dev/null || true
  systemctl disable unattended-upgrades.service 2>/dev/null || true

  info "Updating system..."
  apt-get update -qq
  apt-get full-upgrade -y -qq

  info "Installing packages..."
  apt-get install -y -qq cifs-utils mpd mpc fish git python3-dev jq > /dev/null
}

setup_config_files() {
  info "Installing config files..."
  find "$CONFIG_DIR/etc" -type f -print0 | while IFS= read -r -d '' src; do
    dest="/${src#"$CONFIG_DIR/"}"
    install -Dm644 "$src" "$dest"
  done
  find "$CONFIG_DIR/usr" -type f -print0 | while IFS= read -r -d '' src; do
    dest="/${src#"$CONFIG_DIR/"}"
    install -Dm755 "$src" "$dest"
  done
}

setup_boot() {
  info "Configuring boot..."
  if ! grep -q 'panic=1' /boot/firmware/cmdline.txt 2>/dev/null; then
    sed -i 's/$/ panic=1 boot.panic_on_fail/' /boot/firmware/cmdline.txt
  fi

  # Larger SPI buffer for full-frame e-ink updates on the Inky Impression 7.3".
  # Default 4096 is too small; 128K avoids partial-frame transfers.
  # Ref: https://github.com/pimoroni/inky?tab=readme-ov-file#install-stable-library-from-pypi
  if ! grep -q 'spidev.bufsiz' /boot/firmware/cmdline.txt 2>/dev/null; then
    sed -i 's/$/ spidev.bufsiz=131072/' /boot/firmware/cmdline.txt
  fi
  replace_block /boot/firmware/config.txt "$CONFIG_DIR/boot/firmware/config.txt.fragment"
}

setup_storage() {
  info "Configuring storage..."
  grep -q '/tmp.*tmpfs' /etc/fstab || \
    echo 'tmpfs /tmp tmpfs nosuid,nodev,size=64M 0 0' >> /etc/fstab

  if grep -E '^\S+\s+/\s' /etc/fstab | grep -qv 'noatime'; then
    sed -i '/^\S\+\s\+\/\s/s/defaults/defaults,noatime/' /etc/fstab
  fi
  if grep -E '^\S+\s+/\s' /etc/fstab | grep -qv 'commit='; then
    sed -i '/^\S\+\s\+\/\s/s/noatime/noatime,commit=60/' /etc/fstab
  fi

  replace_block /etc/fstab "$CONFIG_DIR/generated/smb.fstab.fragment"
  mkdir -p /mnt/homelab-media

  if [[ ! -f /root/.smb-credentials ]]; then
    printf 'username=\npassword=\n' > /root/.smb-credentials
    info "IMPORTANT: Edit /root/.smb-credentials with actual NAS credentials"
  fi
  chmod 400 /root/.smb-credentials
}

setup_users() {
  info "Configuring users..."
  getent group homelab-media >/dev/null || groupadd -g "$HOMELAB_MEDIA_GID" homelab-media

  id $USERNAME >/dev/null 2>&1 || fatal "User $USERNAME must be created by rpi-imager"
  usermod -aG audio,gpio,spi,i2c,homelab-media $USERNAME
}

setup_inkypi() {
  info "Setting up InkyPi..."

  # Pinned versions — update SHAs after testing new releases.
  local inkypi_sha="73c21a1ba5b565cace7d65aa8f788712067644ab"
  local immich_sha="af6b48e98fd0b5a842ce20b8e6cd61af41908749"
  local hardwarebuttons_sha="e208ed63b767278262c8dcd957ec890a22001046"

  if [[ ! -d /opt/inkypi/.git ]]; then
    git clone https://github.com/fatihak/InkyPi.git /opt/inkypi
    git -C /opt/inkypi checkout "$inkypi_sha"
    bash /opt/inkypi/install/install.sh < /dev/null
  fi

  local plugins_dir="/usr/local/inkypi/src/plugins"

  # Install plugins pinned to specific commits.
  if [[ ! -d "$plugins_dir/immich" ]]; then
    git clone https://github.com/doowylloh88/InkyPi-Immich "$plugins_dir/immich"
    git -C "$plugins_dir/immich" checkout "$immich_sha"
  fi

  if [[ ! -d "$plugins_dir/hardwarebuttons" ]]; then
    git clone https://github.com/RobinWts/InkyPi-Plugin-hardwarebuttons "$plugins_dir/hardwarebuttons"
    git -C "$plugins_dir/hardwarebuttons" checkout "$hardwarebuttons_sha"
  fi

  # Hardwarebuttons requires patching InkyPi core to register plugin blueprints.
  # Ref: https://github.com/RobinWts/InkyPi-Plugin-hardwarebuttons#step-2-patch-core-files
  if ! grep -q 'register_plugin_blueprints' /usr/local/inkypi/src/inkypi.py 2>/dev/null; then
    bash "$plugins_dir/hardwarebuttons/patch-core.sh"
  fi

  # FIXME: Remove once fixed upstream: https://github.com/pimoroni/inky/issues/258 (and then inkypi)
  local inky_e673="/usr/local/inkypi/venv_inkypi/lib/python3.13/site-packages/inky/inky_e673.py"
  if grep -qF 'self._spi_bus.xfer3(data)' "$inky_e673"; then
    sed -i 's/            self._spi_bus.xfer3(data)/            for _i in range(((len(data) - 1) \/\/ _SPI_CHUNK_SIZE) + 1):\n                _off = _i * _SPI_CHUNK_SIZE\n                self._spi_bus.xfer(data[_off:_off + _SPI_CHUNK_SIZE])/' "$inky_e673"
  elif ! grep -qF '_SPI_CHUNK_SIZE' "$inky_e673"; then
    fatal "inky SPI chunk patch: expected pattern not found — upstream may have changed"
  fi

  # Image saturation: 0 gives vivid colors (default 0.5 is washed out). Ref: https://github.com/fatihak/InkyPi/issues/502
  local device_cfg="/opt/inkypi/src/config/device.json"
  jq \
    --arg tz "$INKYPI_TIMEZONE" \
    --arg tf "$INKYPI_TIME_FORMAT" \
    --argjson sat "$INKYPI_INKY_SATURATION" \
    '.timezone = $tz | .time_format = $tf | .image_settings.inky_saturation = $sat' \
    "$device_cfg" > "$device_cfg.tmp" && mv "$device_cfg.tmp" "$device_cfg"

  # Immich plugin reads IMMICH_KEY from the environment
  if [[ ! -f /etc/inkypi.env ]]; then
    printf 'IMMICH_KEY=\n' > /etc/inkypi.env
    info "IMPORTANT: Edit /etc/inkypi.env with actual Immich API key"
  fi
  chmod 600 /etc/inkypi.env
}

setup_services() {
  info "Configuring services..."
  systemctl disable --now bluetooth.service    || true
  systemctl disable --now avahi-daemon.service || true
  systemctl daemon-reload
  sysctl --system > /dev/null
  systemctl enable mpd
  systemctl restart mpd
  systemctl restart sshd
}

setup_packages
setup_config_files
setup_boot
setup_storage
setup_users
setup_inkypi
setup_services

success "Setup complete. Reboot to apply kernel params and config.txt changes."
