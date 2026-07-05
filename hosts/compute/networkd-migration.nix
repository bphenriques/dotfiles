# ============================================================================================
# ON HOLD — NOT imported anywhere. Reference/staging for migrating compute's networking from
# the legacy scripted stack (networking.bonds/bridges/interfaces + dhcpcd) to systemd-networkd.
#
# SCOPE
#   Replaces, in one module: the bond0 + DHCP config in hardware/default.nix, and the
#   compute-microvm bridge + tap-enslave in microvm/default.nix. When activated: import this
#   from hosts/compute/default.nix and DELETE from the other files:
#     - hardware/default.nix: networking.bonds, networking.interfaces.*, networking.dhcpcd.*,
#       the systemd.services.dhcpcd ordering block, networking.interfaces.*.wakeOnLan
#     - microvm/default.nix: networking.bridges.*, networking.interfaces.<bridge>.*
#     - share-vm/microvm.nix: the microvm.binScripts.tap-up hook (networkd enslaves the tap)
#   networking.nat stays as-is (backend-agnostic).
#
# BENEFITS
#   - Retires the dhcpcd-races-the-slow-bond workaround (the after=bond0.device ordering hack
#     + noipv4ll) — networkd orders on device appearance natively.
#   - Parity with share-vm (already networkd), one stack to reason about.
#   - Declarative tap enslaving replaces the imperative binScripts.tap-up hook.
#   - Fixes today's miimon=0 (active-backup with no link monitoring ⇒ no failover on cable pull).
#
# CONS / RISK
#   - Lockout-prone: a bad net config drops remote access. Apply via `nixos-rebuild boot` + reboot
#     with console/JetKVM (192.168.1.195) ready to roll back a generation. NOT remote-only.
#   - Three items need confirming ON THE BOX, not assumed:
#       * bond primary-slave: networkd's preferred-active handling is less clean than iproute2's
#         `primary=enp1s0`; the pinned MAC makes enp1s0 the natural active — verify `networkctl
#         status bond0` reselect behaviour.
#       * WoL: confirm `ethtool enp1s0 | grep Wake-on` after switch. enp2s0's *permanent* MAC is
#         unknown (both read the bond MAC while enslaved), hence name-matching below.
#       * miimon 0 → 100ms is a deliberate behaviour change, not a like-for-like port.
#   - MAC pinning is load-bearing: the DHCP reservation is keyed on 78:55:36:05:37:c8 (enp1s0's).
#   - IPv6 must be kept: bond0 has a routable global v6 via RA today; IPv6AcceptRA=true preserves it.
#
# VERIFY AFTER SWITCH
#   networkctl status ; ip -br addr show bond0 (still 192.168.1.196 + global v6) ;
#   ping6 a global host ; ./hosts/share-vm/post-deploy-test.sh (microvm must still be bridged).
# ============================================================================================
{ config, ... }:
let
  bridge = config.custom.fleet.computeMicrovm.bridge;
in
{
  networking.useDHCP = false;
  networking.useNetworkd = true;

  systemd.network = {
    enable = true;

    # --- bond0 over enp1s0/enp2s0 (active-backup) ---
    netdevs."10-bond0" = {
      netdevConfig = { Kind = "bond"; Name = "bond0"; MACAddress = "78:55:36:05:37:c8"; }; # pin: keeps the DHCP lease
      bondConfig = { Mode = "active-backup"; MIIMonitorSec = "100ms"; };                    # was miimon=0 (no failover)
    };
    networks."10-enp1s0" = { matchConfig.Name = "enp1s0"; networkConfig.Bond = "bond0"; };
    networks."10-enp2s0" = { matchConfig.Name = "enp2s0"; networkConfig.Bond = "bond0"; };
    networks."10-bond0" = {
      matchConfig.Name = "bond0";
      networkConfig = { DHCP = "ipv4"; IPv6AcceptRA = true; }; # keep the routable global IPv6 (RA)
    };
    links."10-enp1s0" = { matchConfig.OriginalName = "enp1s0"; linkConfig.WakeOnLan = "magic"; };
    links."10-enp2s0" = { matchConfig.OriginalName = "enp2s0"; linkConfig.WakeOnLan = "magic"; };

    # --- compute-microvm bridge (internal, no uplink) + declarative tap enslave ---
    netdevs."20-${bridge.name}".netdevConfig = { Kind = "bridge"; Name = bridge.name; };
    networks."20-${bridge.name}" = {
      matchConfig.Name = bridge.name;
      networkConfig = { Address = "${bridge.gateway}/${toString bridge.prefixLength}"; ConfigureWithoutCarrier = true; };
      linkConfig.RequiredForOnline = "no"; # no carrier until a VM starts — don't hang network-online
    };
    networks."30-vm-tap" = {
      matchConfig.Name = "vm-*"; # replaces microvm binScripts.tap-up
      networkConfig.Bridge = bridge.name;
      linkConfig.RequiredForOnline = "no";
    };
  };
}
