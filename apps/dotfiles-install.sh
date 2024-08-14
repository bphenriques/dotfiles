#!/usr/bin/env sh

usage() {
  echo "dotfiles-install.sh <host> [--dotfiles-location <path>] [-ssh-directory <path>] [--ssh-key-comment <comment>] [--skip-sops-init] [--age-keys-file <path>]

Options:
  --dotfiles-location Sets the dotfiles repository location. Defaults to \$HOME/.dotfiles
  --ssh-directory     Sets the SSH location. Defaults to \$HOME/.ssh
  --ssh-key-comment   Sets the SSH key's comment. Defaults to git user.email
  --age-keys-file     Sets the location of the age private keys. Defaults to \$HOME/.config/sops/age/keys.txt
  --skip-sops-init    Skips the git filter setup. Use if you want to set it later.
"
}

info() { printf '[ \033[00;34m..\033[0m ] %s\n' "$1"; }
success() { printf '[ \033[00;32mOK\033[0m ] %s\n' "$1"; }
warn() { printf '[ \033[01;33mWARN\033[0m ] %s\n' "$1"; }
fatal() { printf '[\033[0;31mFAIL\033[0m] %s\n' "$1" 1>&2; exit 1; }
press_to_continue() { info 'Press any key to continue'; read -r _; }

BRANCH_NAME=add-laptop #FIXME: Remove after we sort-out everything
clone_dotfiles() {
  location="$1"
  ssh_directory="$2"

  # Works for impermanence where the directory is mounted and empty
  if ! test -d "$location" || (find "$location" -maxdepth 0 -empty | read -r _); then
    info "dotfiles - Cloning to $location"
    GIT_SSH_COMMAND="ssh -i "${ssh_directory}/id_ed25519" -o IdentitiesOnly=yes" \
      git clone -b "${BRANCH_NAME}" git@github.com:bphenriques/dotfiles.git "$location"
  fi
  success "dotfiles - available in $location"
}

setup_ssh() {
  directory="$1"
  comment="$2"
  test -z "$comment"  && fatal "SSH comment is empty"

  mkdir -p "${directory}"
  if [ ! -f "$directory"/id_ed25519.pub ]; then
    info "SSH Key - Exporting key-pair on $directory"
    ssh-keygen -t ed25519 -C "$comment" -f "$directory"/id_ed25519
    info 'SSH Key - Copy the public key below to https://github.com/settings/ssh/new'
    cat "$directory"/id_ed25519.pub
    echo ''
    press_to_continue
  else
    success "SSH Key - Key-pair already present in $directory"
  fi
  success "SSH Key - Key-pair available in $directory"
}

set_host() {
  dotfiles_location="$1"
  host="$2"
  host_file_location="$dotfiles_location/.nix-host"

  test -z "${host}" && fatal "host must not be empty!"
  ! test -d "${dotfiles_location}/hosts/${host}" && fatal "No matching '${host}' under '${dotfiles_location}/hosts'"
  if [ -f "$host_file_location" ] && [ "$(cat "$host_file_location")" != "${host}" ]; then
    fatal "There is already a host set that is not identical. Delete ${host_file_location} and try again."
  fi

  echo "${host}" > "${host_file_location}"
  success "Host - Set to ${host}!"
}

does_host_require_secrets() {
  host="$1"
  dotfiles_location="$2"

  yq '.keys[] | anchor' < "${dotfiles_location}"/.sops.yaml | grep -E "^${host}" > /dev/null 2>&1;
}

init_sops_git_filter() {
  dotfiles_location="$1"
  age_keys_file="$2"
  host="$3"
  skip="$4"

  if does_host_require_secrets "${host}" "${dotfiles_location}"; then
    if ! "${dotfiles_location}"/bin/sops-git-filter.sh check "${host}"; then
      if [ "${skip}" != "0" ]; then
        info "Sops Git Filter - Setting up for '${host}'"
        host_public_key="$(host="${host}" yq '.keys[] | select((. | anchor) == env(host))' < "${dotfiles_location}"/.sops.yaml)"
        if age-keygen -y "${age_keys_file}" | grep "${host_public_key}" > /dev/null 2>&1; then
          info "Sops Git Filter - Found matching private-key for ${host} with public key ${host_public_key}"
          "${dotfiles_location}"/bin/sops-git-filter.sh init "${host}"
        else
          warn "Sops Git Filter - No matching age private key for ${host_public_key} under ${age_keys_file}"
        fi
      else
        warn "Sops Git Filter - '$host' has secrets. Will be encrypted until you initialize sops-git-filter.sh"
      fi
    else
      success "Sops Git Filter - Already set for '${host}'"
    fi
  else
    success "Sops Git Filter - Not required for '${host}'"
  fi
}

if [ "$1" = "--help" ]; then
  usage
  exit
fi

dotfiles_location="$HOME"/.dotfiles
ssh_directory="$HOME"/.ssh
ssh_key_comment="$(git config user.email)"
age_keys_file="$HOME/.config/sops/age/keys.txt"
skip_sops_init=1

host="$1"
shift 1
while [ $# -gt 0 ]; do
  case "$1" in
    --dotfiles-location)  dotfiles_location="$2";   shift 2 ;;
    --ssh-directory)      ssh_directory="$2";       shift 2 ;;
    --ssh-key-comment)    ssh_key_comment="$2";     shift 2 ;;
    --age-keys-file)      age_keys_file="$2";       shift 2 ;;
    --skip-sops-init)     skip_sops_init=0;         shift 1 ;;
    *) break ;;
  esac
done

setup_ssh "${ssh_directory}" "${ssh_key_comment}"
clone_dotfiles "${dotfiles_location}" "${ssh_directory}"
set_host "${dotfiles_location}" "${host}"
init_sops_git_filter "${dotfiles_location}" "${age_keys_file}" "${host}" "${skip_sops_init}"

#TODO import gpg key
# cat cenas | gpg --import

# gpg --output private.pgp --armor --export-secret-key 4727729+bphenriques@users.noreply.github.com
# gpg --output public.pgp --armor --export username@email
