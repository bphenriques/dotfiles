{ ... }:
{
  # Docker: probabilly move to somewhere else: https://github.com/iynaix/dotfiles/blob/main/nixos/docker.nix#L19

    # Persist: "/etc/NetworkManager", any .local/state/SOME_FOLDER  "/var/lib/bluetooth" ".local/share/containers"
    # ".config/filezilla"
    #  home = {
        #      cache = [
        #        ".cache/nix"
        #        ".cache/nixpkgs-review"
        #      ];
        #    };
    #
    environment.persistence = {
      "/persist" = {
        hideMounts = true;
        files = [ "/etc/machine-id" ] ++ cfg.root.files;
        directories = [
          "/var/log" # systemd journal is stored in /var/log/journal
        ] ++ cfg.root.directories;

        users.${user} = {
          files = cfg.home.files ++ hmPersistCfg.home.files;
          directories = [
            "projects"
            ".cache/dconf"
            ".config/dconf"
          ] ++ cfg.home.directories ++ hmPersistCfg.home.directories;
        };
      };

      "/persist/cache" = {
        hideMounts = true;
        directories = cfg.root.cache;

        users.${user} = {
          directories = hmPersistCfg.home.cache;
        };
      };
    };
}
