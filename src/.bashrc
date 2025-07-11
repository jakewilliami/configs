#!/usr/bin/env bash

### Session ###

# Set Emacs to default editor
export EDITOR='emacs -nw'
export VISUAL='emacs -nw'

# Add local opt to path
export PATH=$HOME/opt/:$PATH

### Git ###

# Get current branch in git repo
function parse_git_branch() {
	BRANCH=$(git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/\1/')
	if [ ! "${BRANCH}" == "" ]
	then
		STAT=$(parse_git_dirty)
		echo "[${BRANCH}${STAT}]"
	else
		echo ""
	fi
}

# Get current status of git repo
function parse_git_dirty {
	status=$(git status 2>&1 | tee)
	dirty=$(echo -n "${status}" 2> /dev/null | grep "modified:" &> /dev/null; echo "$?")
	untracked=$(echo -n "${status}" 2> /dev/null | grep "Untracked files" &> /dev/null; echo "$?")
	ahead=$(echo -n "${status}" 2> /dev/null | grep "Your branch is ahead of" &> /dev/null; echo "$?")
	newfile=$(echo -n "${status}" 2> /dev/null | grep "new file:" &> /dev/null; echo "$?")
	renamed=$(echo -n "${status}" 2> /dev/null | grep "renamed:" &> /dev/null; echo "$?")
	deleted=$(echo -n "${status}" 2> /dev/null | grep "deleted:" &> /dev/null; echo "$?")
	bits=''
	if [ "${renamed}" == "0" ]; then
		bits=">${bits}"
	fi
	if [ "${ahead}" == "0" ]; then
		bits="*${bits}"
	fi
	if [ "${newfile}" == "0" ]; then
		bits="+${bits}"
	fi
	if [ "${untracked}" == "0" ]; then
		bits="?${bits}"
	fi
	if [ "${deleted}" == "0" ]; then
		bits="x${bits}"
	fi
	if [ "${dirty}" == "0" ]; then
		bits="!${bits}"
	fi
	if [ ! "${bits}" == "" ]; then
		echo " ${bits}"
	else
		echo ""
	fi
}

# Make prompt pretty
PS1="\n\[\033[0;31m\]\342\224\214\342\224\200\$()[\[\033[1;38;5;2m\]\u\[\033[0;1m\]@\033[1;33m\]\h: \[\033[1;34m\]\W\[\033[1;33m\]\[\033[0;31m\]]\[\033[0;32m\] \[\033[1;33m\]\`parse_git_branch\`\[\033[0;31m\]\n\[\033[0;31m\]\342\224\224\342\224\200\342\224\200\342\225\274 \[\033[0;1m\]\$\[\033[0;38m\] "
export PS1


### Aliases ###

# Redefine `ls` (https://github.com/eza-community/eza)
if ! command -v eza > /dev/null 2>&1; then
	alias l1="ls -1"
	alias ll="ls -l"
	alias lll="ls -la"
else
    alias ls="eza"
    alias l1="eza -1"
    alias ll="eza -l"
    alias lll="eza -la"
    alias lsg="eza -l --git"
fi

# Use eza over exa as the latter is deprecated (https://github.com/ogham/exa/issues/1243)
if command -v eza > /dev/null 2>&1; then
	alias exa="eza"
fi

# Alias for `cat` command
if command -v bat > /dev/null 2>&1; then
	alias cat="bat"
fi

# Convenient alias for youtube-dl to preferred tool
if command -v yt-dlp > /dev/null 2>&1; then
    alias youtube-dl="yt-dlp"
fi

# Add alias for `open` command
if command -v open > /dev/null 2>&1; then
	alias o="open"
else
	alias o="xdg-open"
fi

# Add alias for editor
function e() {
	if [[ $# -eq 0 ]]; then
		emacs . &
	else
		emacs "$@"
	fi
}

# Refine du
if command -v dutree > /dev/null 2>&1; then
	alias du="dutree"
	alias du1="dutree --depth=1 --aggr=1M 2> /dev/null"
fi

# Fun aliases
alias please="sudo"
alias shuffle="sort -R"
alias pythong="python"
alias огдшф="julia"

# Open directory aliases
alias open-dir-dl='wget --recursive --level=0 --no-parent --continue --reject "index.html*"' # Use the -P option to set an output directory
alias open-dir-dl-alt='wget --recursive --continue --level=0 --no-parent --reject="index.html*" --no-clobber --convert-links --random-wait --adjust-extension --execute robots=off --user-agent=mozilla'

# Finnish Arch aliases
alias Sammu="echo -e '\u001b[1;34m===>\t\u001b[0;38m\u001b[1;38mShutting down machine\u001b[0;38m'; sleep 5; sync; sudo systemctl poweroff" # Sammu; Finnish: Shutdown
alias Päivitys="echo -e '\u001b[1;34m===>\t\u001b[0;38m\u001b[1;38mStarting system upgrade and rebooting machine\u001b[0;38m'; sleep 5; sync; sudo pacman -Syyu; sudo shutdown -r now" # Päivitys; Finnish: Upgrade
alias Siivous="echo -e '\u001b[1;34m===>\t\u001b[0;38m\u001b[1;38mClearing pacman cache\u001b[0;38m'; sleep 5; sync; sudo pacman -Sc; sudo paccache -r" # Siivous; Finnish: Clear Cache
alias Kaynnistää-uudelleen="echo -e '\u001b[1;34m===>\t\u001b[0;38m\u001b[1;38mRebooting machine\u001b[0;38m'; sleep 5; sync; sudo reboot" # Käynnistää-uudelleen; Finnish: Start Again (Reboot)
alias Shutdown="echo 'Run the alias Sammu'"
alias Cache="echo 'Run the alias Siivous'"
alias Reboot="echo 'Run the alias Kaynnistää-uudelleen'"
alias Upgrade="echo 'Run the alias Päivitys'"


### Functions ###

function get-version() {
	defaults read /Applications/"${1}.app"/Contents/Info CFBundleShortVersionString
}

function view-md() {
	pandoc "${1}" | lynx -stdin
}

function get-status() {
	curl -s -o /dev/null -w "%{http_code}" $1; echo
}
alias http-status-code="get-status"

