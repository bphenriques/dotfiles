{ pkgs, ... }:

{
  fonts.enableFontDir = true;
  fonts.fonts = with pkgs; [
    (nerdfonts.override { fonts = [ "Hack" ]; }) # More available here: https://github.com/ryanoasis/nerd-fonts
  ];
}
