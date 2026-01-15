My work laptop started being overly restrictive which blocks Nix installations.

I could figure out how to deal with that, but I think there will be far more obstacles than the type I have to deal with it.
I will consider a way to generate the `.dotfiles` and then copy them over manually.

1. Install Homebrew:
```shell
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
```

2. Run the homebrew post-install which should update the shell startup to run:
```shell
eval "$(/opt/homebrew/bin/brew shellenv)"
```

3. Generate a key and upload to Github:
```shell
ssh-keygen -t ed25519 -C "your_email@example.com"
```

4. One time:
```
sudo ln -sfn /usr/local/opt/openjdk/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk.jdk
jenv add "$(/usr/libexec/java_home)" # this will always default to the latest version in /Library/Java/JavaVirtualMachines
jenv add /Library/Java/JavaVirtualMachines/temurin-17.jdk/Contents/Home/
jenv global 17
```

Setup docker