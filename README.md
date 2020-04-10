# Description

Yet another workspace installer. Saw many, took inspiration of some and
got confused by others. In the end I decided to learn by doing one myself
with the following guidelines:
* Portable
* Minimalistic which means:
    * Using `stow` to manage dotfiles seamlessly.
    * Automate `Brewfile` installation.
* Extensible as (unfortunately) some programs are not as seamless as `brew install <module>` 
  followed by `stow <module>`.
* Idempotent as much as possible so that I can run the installer as many times as I want.
* Consistency between the operating systems I use.

# Installation

In order to install modules:
```sh
$ ./installer.sh [MODULE 1] [MODULE 2] [MODULE N]
```

After this:
1. Setup sensitive information.
2. You may have to manually open the UI applications in order to set them up.

# Dev

## Requirements

* [shellcheck](https://github.com/koalaman/shellcheck)

## Commands

Check the `README.md` file for the available commands.

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