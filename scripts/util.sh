#!/bin/sh

info() {
    # shellcheck disable=SC2059
    printf '\r  [ \033[00;34m..\033[0m ] %s\n' "$1"
}

success() {
    # shellcheck disable=SC2059
    printf '\r\033[2K  [ \033[00;32mOK\033[0m ] %s\n' "$1"
}

fail() {
    # shellcheck disable=SC2059
    printf '\r\033[2K  [\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2 # Redirect to stderror
    exit 1
}

append_if_absent() {
    line="$1"
    file="$2"
    
    # Make sure that file exists
    touch "$file"

    # Append if absent
    grep -qF -- "$line" "$file" || echo "$line" >> "$file"
}
