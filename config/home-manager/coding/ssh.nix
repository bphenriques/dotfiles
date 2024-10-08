{ servers, ... }:
{
  programs.ssh = {
    enable = true;
    serverAliveInterval = 60;
    matchBlocks = {
      home-nas = {
        inherit (servers.home-nas) hostname;
        inherit (servers.home-nas.ssh) user port;
      };
      pi-zero = {
        inherit (servers.pi-zero) hostname;
        inherit (servers.pi-zero.ssh) user;
      };
    };
  };
}