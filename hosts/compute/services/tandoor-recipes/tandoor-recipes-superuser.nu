#!/usr/bin/env nu

# Creates Tandoor Recipes superuser if it doesn't exist.
# Uses Django's manage.py commands for migrations and user creation.
#
# NOTE: The initial space must be created manually via the web setup wizard.
# After that, OIDC users auto-join space 1 via SOCIAL_DEFAULT_ACCESS.

let username = open $env.TANDOOR_ADMIN_USERNAME_FILE | str trim
let password = open $env.TANDOOR_ADMIN_PASSWORD_FILE | str trim

def main [] {
  print "Running database migrations..."
  tandoor-recipes migrate --noinput

  print $"Ensuring superuser '($username)' exists..."
  with-env {
    DJANGO_SUPERUSER_USERNAME: $username
    DJANGO_SUPERUSER_PASSWORD: $password
  } {
    try { tandoor-recipes createsuperuser --noinput }
  }

  print "Superuser setup complete"
}
