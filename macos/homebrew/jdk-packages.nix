# Notes:
# - Should be set per project and not OS-wide.
# - Move to a shared cross-platform module.
{
  homebrew.enable = true;
  
  homebrew.taps = [
    "adoptopenjdk/openjdk"
  ];
  homebrew.casks = [
    "adoptopenjdk8"
    "adoptopenjdk11"
  ];
}
