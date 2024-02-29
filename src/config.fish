# Alias for editor
function e
	if test (count $argv) -eq 0
		emacs . &
	else
		emacs $argv
	end
end

# Open tmux session
if status --is-interactive
	if ! set -q TMUX
		# https://unix.stackexchange.com/a/176885/372726
		exec tmux new-session -A -s main
	end
end

# Alias for opening things
if command -v open > /dev/null
	abbr -a o open
else
	abbr -a o xdg-open
end

# Use eza over ls if available (https://github.com/eza-community/eza)
if command -v eza > /dev/null
	abbr -a l 'eza'
	abbr -a ls 'eza'
	abbr -a ll 'eza -l'
	abbr -a lll 'eza -la'
    abbr -a lsg 'eza -l --git'
else
	abbr -a l 'ls'
	abbr -a ll 'ls -l'
	abbr -a lll 'ls -la'
end

# Use eza over exa as the latter is deprecated (https://github.com/ogham/exa/issues/1243)
if command -v eza > /dev/null
	abbr -a exa 'eza'
end

# Convenient alias for youtube-dl to preferred tool
if command -v yt-dlp > /dev/null
	abbr -a youtube-dl 'yt-dlp'
end

# Use dutree over du if available (https://github.com/nachoparker/dutree)
if command -v dutree > /dev/null
	abbr -a du 'dutree'
	abbr -a du1 'dutree --depth=1 --aggr=1M'
end

# Fish git prompt
set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate ''
set __fish_git_prompt_showupstream 'none'
set -g fish_prompt_pwd_dir_length 3

# colored man output
# from http://linuxtidbits.wordpress.com/2009/03/23/less-colors-for-man-pages/
setenv LESS_TERMCAP_mb \e'[01;31m'       # begin blinking
setenv LESS_TERMCAP_md \e'[01;38;5;74m'  # begin bold
setenv LESS_TERMCAP_me \e'[0m'           # end mode
setenv LESS_TERMCAP_se \e'[0m'           # end standout-mode
setenv LESS_TERMCAP_so \e'[38;5;246m'    # begin standout-mode - info box
setenv LESS_TERMCAP_ue \e'[0m'           # end underline
setenv LESS_TERMCAP_us \e'[04;38;5;146m' # begin underline

# Use fd (https://github.com/sharkdp/fd)
setenv FZF_DEFAULT_COMMAND 'fd --type file --follow'
setenv FZF_CTRL_T_COMMAND 'fd --type file --follow'
setenv FZF_DEFAULT_OPTS '--height 20%'

# Fish should not add things to clipboard when killing
# See https://github.com/fish-shell/fish-shell/issues/772
set FISH_CLIPBOARD_CMD "cat"

