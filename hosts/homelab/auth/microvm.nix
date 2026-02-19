{ config, pkgs, lib, self, ... }:
let
  networking = import ../networking.nix;
in
{
  # Network configuration using systemd-networkd
  # Matches the virtio NIC by type (interface names vary)
  networking.useDHCP = false;
  networking.useNetworkd = true;
  systemd.network.networks."20-lan" = {
    matchConfig.Type = "ether";
    networkConfig = {
      Address = "${networking.vm.auth.ip}/24";
      Gateway = networking.bridge.gateway;
      DNS = builtins.head networking.cloudflare.nameservers;
      DHCP = "no";
    };
  };

  # MicroVM settings
  microvm = {
    hypervisor = "qemu";
    vcpu = 2;
    mem = 1024;  # 1GB RAM

    interfaces = [{
      type = "tap";
      id = "vm-auth";
      mac = networking.vm.auth.mac;
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
