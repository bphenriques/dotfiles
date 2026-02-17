{ config, pkgs, lib, self, ... }:
let
  authVmIp = "10.20.0.10";
  hostBridgeIp = "10.20.0.1";
in
{
  # Network configuration
  networking = {
    useDHCP = false;
    interfaces.eth0.ipv4.addresses = [{ address = authVmIp; prefixLength = 24; }];
    defaultGateway = { address = hostBridgeIp; interface = "eth0"; };
  };

  # MicroVM settings
  microvm = {
    hypervisor = "qemu";
    vcpu = 2;
    mem = 1024;  # 1GB RAM

    interfaces = [{
      type = "tap";
      id = "vm-auth";
      mac = "02:00:00:00:00:01";
    }];

    shares = [{
      source = "/nix/store";
      mountPoint = "/nix/.ro-store";
      tag = "ro-store";
      proto = "virtiofs";
    }];

    volumes = [{
      image = "auth.img";
      mountPoint = "/var";
      size = 2048;  # 2GB for /var
    }];
  };
}
