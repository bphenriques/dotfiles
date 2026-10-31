{ config, lib, self, ... }:
let
  guests = import ./guests;
in
{
  my.microvm.host = {
    enable = true;
    uplink = "eth0";
    adminUser = config.users.users.bphenriques.name;
    inherit (guests) bridge;
    guests = lib.mkMerge [
      guests.guests
      {
        agent-alfred-vm.egress = {
          allowHostPorts = [ config.fleet.ai.endpoint.port config.fleet.ai.speechEndpoint.port ];   # Ollama and speech-to-text
          allowLan = [
            { host = "storage"; ports = [ 445 ]; }   # its NAS doors
            { host = "compute"; ports = [ self.nixosConfigurations.compute.config.selfhost.apps.gitea.ssh.port ]; }   # its own repo
          ];
        };
      }
    ];
  };
}
