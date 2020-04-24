# Screenshots

TODO

# Description

Personal workspace installer. Saw many, took inspiration of some and
got confused by others. In the end I decided to learn by doing one myself
with the following guidelines:
* Cross-platform to keep workflow consistency between different OS.
* Sensible settings and avoid being too couple to my own settings.
* Simplify installation as possible which means:
    * Using `stow` to manage dotfiles seamlessly.
    * Automate `Brewfile` installation.
    * Unfortunately some programs are not installed with just `brew install <module>` followed by `stow <module>`, therefore had to get creative.
* Idempotenty so the installer can be run as many times as possible.

# Installation

In order to install modules:
```sh
$ ./installer.sh [MODULE 1] [MODULE 2] [MODULE N]
```

After this:
1. Setup sensitive information.
2. You may have to manually open the UI applications in order to set them up.

# Development

## Requirements

* [shellcheck](https://github.com/koalaman/shellcheck)

## Commands

Check the `README.md` file for the available commands.

# Modules

## Allacrity

### Keybindings

| Shortcut | Command                        |
|----------|--------------------------------|
| Cmd-N    | Open new window                |

## Tmux

* Made sure that the default `#!Z` indicators are present.
* Added `Ψ` as indicator that the panels in a window have their input syncronized.
* Some tweaks to the theme.

### Keybindings

| Shortcut    | Command                        |
|-------------|--------------------------------|
| `C-a`       | Prefix                         |
| `C-a -`     | Split Horizontally             |
| `C-a \|`    | Split Vertically               |
| `C-a b`     | Toggle Broadcast input         |
| `C-a h`     | Go to pane on the Left         |
| `C-a j`     | Go to pane up                  |
| `C-a k`     | Go to the pane down            |
| `C-a l`     | Go to pane on the right        |
| `C-a C`     | Open and and rename new window |

# TODO TODO

* TODO: Setup separate encrypted git for sensitive information
* TODO: Settle on a font and setup it automatically: https://github.com/romkatv/powerlevel10k#meslo-nerd-font-patched-for-powerlevel10k
* TODO: Improve idempotence of some commands.
* TODO: Maybe especify a version of each component to avoid issues -> leads to manual maintenance.
* https://www.digitalocean.com/community/tutorials/an-introduction-to-managing-secrets-safely-with-version-control-systems
* https://medium.com/@GeorgiosGoniotakis/how-to-keep-your-repositorys-sensitive-data-secure-using-git-secret-c1ddc28cb985
* https://github.com/StackExchange/blackbox
* https://github.com/sobolevn/git-secret
* https://thoughtbot.com/blog/my-life-with-neovim
* https://github.com/jarun/Buku
* https://github.com/moncho/dry
* https://github.com/go-jira/jira
* https://github.com/erroneousboat/slack-term
* https://github.com/zsh-users/zsh-autosuggestions
* https://github.com/tmuxinator/tmuxinator