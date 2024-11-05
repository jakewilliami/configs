#!/usr/bin/env bash
# 
# Script to sync dotfiles with the git repository.
# Can be used to sync them from your machine to the git
# repository, or from the repo to your local machine.
# 
# Usage:
# You can sync these files from the git repository
# to your machine using the "local" parameter:
# $ ./sync-dotfiles.sh local
# 
# You can sync the dotfiles from your machine, locally,
# to the git repository by either providing no arguments,
# or the "remote" argument:
# $ ./sync-dotfiles.sh
# $ ./sync-dotfiles.sh remote
# 
# This helper script was written by Jake Ireland
# (jakewilliami@icloud.com) in Winter, 2022.

# Define usage
USAGE="\
USAGE:
    ./sync-dotfiles.sh [remote|local]

ARGS:
    remote: pull dotfiles locally and update them in the repo
    local:  pull dotfiles from the repo and sync them locally
"

# Match command-line arguments to appropriate usage
case "$1" in
    (remote) mode="remote";;
    (local)  mode="local";;
    (-h)     echo "$USAGE" && exit 0;;
    (*)      echo "$USAGE" && exit 1;;
esac

if [ -z "$mode" ]; then
    echo "ERROR: Mode not set"
    echo "$USAGE"
    exit 1
fi

# Define the dotfiles we want to sync
# NOTE: by default this will pull from the src/ directory, however you can put an optional subdirectory from which to pull after a `:' delimiter.
declare -a DOTFILES=(
    "$HOME/.bash_profile"
    "$HOME/.bashrc"
    "$HOME/.emacs.d/init.el:.emacs.d/"
    "$HOME/.emacs.local/llvm-mode.el:.emacs.local/"
    "$HOME/.emacs.snippets/fundamental-mode/mit.snippet:.emacs.snippets/fundamental-mode/"
    "$HOME/.emacs.snippets/rust-mode/main.snippet:.emacs.snippets/rust-mode/"
    "$HOME/.vimrc"
    "$HOME/.tmux.conf"
    "$HOME/.alacritty.yml"
    "$HOME/.config/fish/config.fish"
    "$HOME/.config/btop/btop.conf"
)

# Main script
SCRIPT_DIR="$( cd -- "$( dirname -- "${BASH_SOURCE[0]:-$0}"; )" &> /dev/null && pwd 2> /dev/null; )"
OUT_OF_SYNC=false
for fsrc in "${DOTFILES[@]}"; do
    # Split the input by delimiter
    IFS=':' read -ra parts <<< "$fsrc"

    # Extract optional subdir from input with src as default
    fsrc="${parts[0]}"
    subd="src"
    if (("${#parts[@]}" == 2)); then
        subd="$subd/${parts[1]}"
    fi

    # Construct the input or destination file path
    fdst="$SCRIPT_DIR/$subd/$(basename "$fsrc")"

    # Check that we need to update resource before doing so
    if [ ! -f "$fdst" ] || ! cmp -s "$fsrc" "$fdst"; then
        if ! $OUT_OF_SYNC; then
	        OUT_OF_SYNC=true
        fi
		if [ "$mode" = "remote" ]; then
			cp -vi "$fsrc" "$(dirname "$fdst")"
		elif [ "$mode" = "local" ]; then
            if [ ! -d "$(dirname "$fsrc")" ]; then
                mkdir -p "$(dirname "$fsrc")"
            fi
			cp -vi "$fdst" "$fsrc"
        else
			echo "Unknown '$MODE'" && exit 1
		fi
    fi
done

if ! $OUT_OF_SYNC; then
   echo "Already up to date."
fi
