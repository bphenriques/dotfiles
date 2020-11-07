#!/bin/sh
set -e

git_config_directory="$XDG_CONFIG_HOME/git"
git_sensitive_location="$git_config_directory/gitconfig.sensitive"

if [ ! -f "$git_sensitive_location" ]; then
   echo "Creating placeholder $git_sensitive_location..."
   touch "$git_config_directory/.gitconfig.sensitive"
fi
