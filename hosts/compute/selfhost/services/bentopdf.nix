{ config, ... }:
{
  selfhost.apps.bentopdf.enable = true;
  selfhost.services.bentopdf = {
    access.allowedGroups = [ config.selfhost.groups.users ];
    forwardAuth.enable = true;
  };
}
