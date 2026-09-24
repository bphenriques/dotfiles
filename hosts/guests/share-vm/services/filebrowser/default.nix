{ config, lib, inputs, pkgs, private, shareVm, ... }:
let
  inherit (shareVm) filesRoot dataRoot;
  fbq = config.services.filebrowser-quantum;

  # Machine-only credential the reconciler uses; generated here so it never enters sops or the store.
  adminDir = "${dataRoot}/.filebrowser";
  adminPasswordFile = "${adminDir}/admin-password";

  localhostOnly = {
    IPAddressDeny = "any";
    IPAddressAllow = "localhost";
  };

  # Folders from private settings, plus the unlisted sentinel derived from the option so the two
  # cannot drift apart: the module checks every scope resolves, including that one.
  arrangedDirs = private.settings.filebrowser.folders ++ [ (lib.removePrefix "/" fbq.unlistedScope) ];

  # That scope check is global: one user pointing at a missing directory fails ExecStartPre and takes
  # the share down for everyone. Catch the typo here, where it costs a rebuild instead of an outage.
  badScopes = lib.filterAttrs
    (_: u: u.scope != "/" && !(lib.elem (lib.removePrefix "/" u.scope) arrangedDirs))
    private.settings.filebrowser.users;
in
{
  imports = [ inputs.selfhost-nix.nixosModules.filebrowser-quantum ];

  assertions = [{
    assertion = badScopes == { };
    message = "share-vm filebrowser: scope with no matching folder: "
      + lib.concatStringsSep ", " (lib.mapAttrsToList (n: u: "${n} -> ${u.scope}") badScopes)
      + ". Add it to filebrowser.folders in private settings, or fix the name.";
  }];

  services.filebrowser-quantum = {
    enable = true;
    user = "filebrowser";
    group = "filebrowser";
    # Default is the tmpfs root: thumbnails for a 40GB share would accumulate in the VM's RAM.
    stateDir = "${dataRoot}/filebrowser-quantum";
    inherit adminPasswordFile;
    inherit (private.settings.filebrowser) users;
    source = {
      path = filesRoot;
      # the data volume is a filesystem root; the indexer cannot read lost+found
      rules = [ { folderPath = "/lost+found"; } ];
    };
    unlistedScope = "/.unlisted"; # moot: BasicAuth admits only listed users, safe sentinel if not
    settings = {
      server = {
        numImageProcessors = 2; # one per vCPU: previews must not starve the request path
      };
      frontend = {
        name = "Share";
        loginIcon = "${./branding/img/logo.svg}";
        favicon = "${./branding/img/logo.svg}";
        disableDefaultLinks = true;
        disableUsedPercentage = true;
      };
      userDefaults.listing = {
        viewMode = "gallery"; # the share is photos; a file list is the wrong default
        singleClick = true;
        showHidden = false;
      };
    };
  };

  # Keep the pre-migration account name: the data volume's files are owned by its uid, and the laptop
  # curates over sshfs as this user. Renaming would orphan 40GB and break the curation mount.
  users.users.filebrowser = {
    isSystemUser = true;
    group = "filebrowser";
  };
  users.groups.filebrowser = { };

  systemd.services.filebrowser-admin-password = {
    description = "Generate the FileBrowser admin password";
    requiredBy = [ "filebrowser-quantum.service" ];
    before = [ "filebrowser-quantum.service" ];
    unitConfig.RequiresMountsFor = [ dataRoot ];
    serviceConfig = {
      Type = "oneshot";
      RemainAfterExit = true;
    };
    script = ''
      install -d -m 0700 ${adminDir}
      [ -s ${adminPasswordFile} ] || {
        umask 077
        ${lib.getExe pkgs.openssl} rand -hex 32 > ${adminPasswordFile}
      }
    '';
  };

  # Host-created storage (folders declared privately); content is curated via laptop sshfs.
  systemd.tmpfiles.rules = builtins.map
    (f: "d ${filesRoot}/${f} 0700 ${fbq.user} ${fbq.group} -")
    arrangedDirs;

  # the data volume holds others' uploads: data, never an execution path
  fileSystems.${filesRoot}.options = [ "noexec" "nosuid" "nodev" ];
  # localhost-only egress: a compromised FileBrowser cannot exfiltrate or reach tailnet/LAN. The
  # reconciler only ever talks to the server on loopback, so it is held to the same rule.
  systemd.services.filebrowser-quantum.serviceConfig = localhostOnly;
  systemd.services.filebrowser-quantum-configure.serviceConfig = localhostOnly;
}
