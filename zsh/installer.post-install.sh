#!/bin/sh
set -e

local_env="~/.zshenv.local"

if [ ! -f "$local_env" ]; then
   echo "Creating $local_env..."
   touch "$local_env"
fi
