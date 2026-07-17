# Static TURN credentials for RomM/EmulatorJS netplay, shared by the coturn server (coturn.nix) and the
# EmulatorJS client config (romm.nix) so they can't silently desync. Plaintext is acceptable: the relay is
# LAN/WG-only (firewall-scoped in coturn.nix) and these are non-sensitive.
{
  username = "romm";
  credential = "romm-netplay";
}
