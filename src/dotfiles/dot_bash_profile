#!/usr/bin/env bash

[[ -e "$HOME/.bashrc" ]] && . "$HOME/.bashrc"
[[ -e "$HOME/.cargo/env" ]] && . "$HOME/.cargo/env"

# Set up Homebrew shell environment
#
# Also switches to using brew-installed bash as
# default shell.
#
# As bash is no longer the default shell on macOS
# (replaced by zsh), we use brew-installed bash as
# it will be up to date (Apple no longer ships an
# updated version of bash).
#
# Refs:
#   https://brew.sh
#   https://unix.stackexchange.com/a/750210
#   https://support.apple.com/kb/HT208050
#   https://discussions.apple.com/thread/250722978
#   https://stackoverflow.com/a/77052639
if [[ -e "/usr/local/bin/brew" ]]; then
    eval "$(/usr/local/bin/brew shellenv)"
elif [[ -e "/opt/homebrew/bin/brew" ]]; then
    eval "$(/opt/homebrew/bin/brew shellenv)"
else
    echo "WARNING: no Homebrew configuration found"
fi

# LLVM
# https://stackoverflow.com/a/42730721
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"

# Upside-down face ˙ᵕ˙
echo "( .-.)"

#  ╱|、
# (˚ˎ 。7
#  |、˜〵
# じしˍ,)ノ
