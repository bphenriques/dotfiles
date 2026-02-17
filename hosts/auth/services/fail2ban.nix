_: {
  services.fail2ban = {
    enable = true;
    maxretry = 3;
    bantime = "1h";
    bantime-increment.enable = true;
    jails.sshd = {
      settings = {
        enabled = true;
        port = 22;
        filter = "sshd";
        maxretry = 3;
      };
    };
  };
}