{ config, pkgs, ... }:
let
  inherit (config.custom.fleet) ai;
in
{
  environment.systemPackages = [
    (pkgs.writeShellApplication {
      name = "ollama-models";
      runtimeInputs = [ pkgs.curl pkgs.jq pkgs.util-linux ];
      runtimeEnv.OLLAMA_API = "http://${ai.endpoint.host}:${toString ai.endpoint.port}";
      text = builtins.readFile ./ollama-models.sh;
    })
  ];
}
