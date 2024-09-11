{ lib, pkgs, ... }:
pkgs.writeShellApplication {
  name = "frg";
  runtimeInputs = with pkgs; [ ripgrep fzf ];
  text = lib.fileContents ./src/frg.sh;

  meta = with lib; {
    description = "fzf+ripgrep";
    maintainers = with maintainers; [ bphenriques ];
    license = with licenses; [ mit ];
    mainProgram = "frg";
  };
}
