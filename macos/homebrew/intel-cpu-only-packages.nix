# Until a better automated way is in-place, selectively include this file.
# Atm, brew is configured to install apps using Rosetta 2.
{
  homebrew.enable = true;
  
  homebrew.taps = [ "homebrew/cask" ];
  homebrew.casks = [
    "docker"                    # Containers.
    "intellij-idea-ce"          # IDE for JVM projects.
  ];
}
