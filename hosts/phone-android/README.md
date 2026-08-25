# Phone (Android)

Minimal setup script for my Android phone running [Termux](https://termux.dev), installed via F-Droid.

## Setup

1. Run the following idempotent script in Termux:

```sh
pkg install -y curl && curl -fsSL https://raw.githubusercontent.com/bphenriques/dotfiles/main/hosts/phone-android/setup.sh | bash
```

2. Add the printed public key to `custom.fleet.ssh.authorizedKeys` in [`hosts/shared.nix`](../shared.nix):

```sh
ssh-keygen -t ed25519 -C phone-android
```
