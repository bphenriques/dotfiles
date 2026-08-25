#!/usr/bin/env bash
# Idempotent Termux setup. Run inside Termux:
#   pkg install -y curl && curl -fsSL https://raw.githubusercontent.com/bphenriques/dotfiles/main/hosts/phone-android/setup.sh | bash
set -euo pipefail

# Wrapped so a truncated download cannot half-apply.
main() {
  [[ -n ${TERMUX_VERSION:-} ]] || {
    echo "Not running inside Termux" >&2
    exit 1
  }

  pkg install -y openssh >/dev/null
  mkdir -p "$HOME/.termux" "$HOME/.ssh"

  # base16 OneDark, matching stylix.
  cat >"$HOME/.termux/colors.properties" <<'EOF'
background=#282c34
foreground=#abb2bf
cursor=#abb2bf
color0=#282c34
color1=#e06c75
color2=#98c379
color3=#e5c07b
color4=#61afef
color5=#c678dd
color6=#56b6c2
color7=#abb2bf
color8=#545862
color9=#e06c75
color10=#98c379
color11=#e5c07b
color12=#61afef
color13=#c678dd
color14=#56b6c2
color15=#c8ccd4
EOF

  # Address, not name: the phone has no /etc/hosts and the router does not resolve LAN names.
  cat >"$HOME/.ssh/config" <<'EOF'
Host laptop
  HostName 192.168.1.121
  User bphenriques
EOF
  chmod 700 "$HOME/.ssh"
  chmod 600 "$HOME/.ssh/config"

  grep -q "alias laptop=" "$HOME/.bashrc" 2>/dev/null \
    || echo "alias laptop='ssh laptop'" >>"$HOME/.bashrc"

  [[ -f $HOME/.ssh/id_ed25519 ]] \
    || ssh-keygen -t ed25519 -N "" -C phone-android -f "$HOME/.ssh/id_ed25519" >/dev/null

  termux-reload-settings
  printf '\nPublic key:\n\n%s\n\n' "$(cat "$HOME/.ssh/id_ed25519.pub")"
}

main "$@"
