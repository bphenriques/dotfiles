# Tandoor user provisioning (idempotent).
#
# Creates local users with password auth and assigns them to the default space
# with the configured permission group. Strips comment permissions from the
# guest group to make it truly read-only.
#
# Environment variables:
#   TANDOOR_USERS_CONFIG   - Path to JSON config with user entries
#   CREDENTIALS_DIRECTORY  - systemd credentials directory with password files
import json, os

from django.contrib.auth import get_user_model
from django.contrib.auth.models import Group, Permission
from cookbook.models import Space, UserSpace

User = get_user_model()

config = json.load(open(os.environ["TANDOOR_USERS_CONFIG"]))
creds_dir = os.environ.get("CREDENTIALS_DIRECTORY", "")
space = Space.objects.first()
if not space:
    raise RuntimeError("No space found. Tandoor may not be fully initialized yet")

for entry in config:
    username = entry["username"]
    email = entry.get("email", "")
    group_name = entry["group"]

    password_file = os.path.join(creds_dir, entry["passwordCredential"])
    password = open(password_file).read().strip()

    user, created = User.objects.get_or_create(username=username, defaults={"email": email, "is_active": True})
    if created:
        print(f"Created user: {username}")
    else:
        print(f"User exists: {username}")

    user.set_password(password)
    user.email = email
    user.save()

    try:
        group = Group.objects.get(name=group_name)
    except Group.DoesNotExist:
        raise RuntimeError(f"Group '{group_name}' not found. Tandoor may not be fully initialized yet")

    # Strip comment permissions from guest group (make it truly read-only)
    if group_name == "guest":
        comment_perms = Permission.objects.filter(codename__contains="comment")
        group.permissions.remove(*comment_perms)

    us, _ = UserSpace.objects.update_or_create(user=user, space=space, defaults={"active": True})
    us.groups.set([group])
    print(f"  Assigned to space with group: {group_name}")

print("User provisioning complete")
