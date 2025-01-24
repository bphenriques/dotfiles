{ lib }:
{
  hexToRGB = hex: lib.strings.removePrefix "#" hex;
}
