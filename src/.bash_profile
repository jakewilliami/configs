#!/usr/bin/env bash

[[ -e "$HOME/.bashrc" ]] && . "$HOME/.bashrc"
[[ -e "$HOME/.cargo/env" ]] && . "$HOME/.cargo/env"

# Switch to using brew-installed bash as default shell
# As bash is no longer the default shell on macOS, we use
# brew-installed bash as it will be updated
#   https://stackoverflow.com/a/77052639
eval "$(/opt/homebrew/bin/brew shellenv)"

echo "( .-.)"

