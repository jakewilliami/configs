# Set Emacs to default editor
setenv EDITOR "emacs -nw"
setenv VISUAL "emacs -nw"

# Add local binary paths
fish_add_path --global --move --path "$HOME/opt/"
fish_add_path --global --move --path "$HOME/.local/bin/"

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
if test -e /usr/local/bin/brew
    eval (/usr/local/bin/brew shellenv)
else if test -e /opt/homebrew/bin/brew
    eval (/opt/homebrew/bin/brew shellenv)
end

#  Cargo
if test -e "$HOME/.cargo/bin/"
    # Sourcing the env file does not work for fish
    # . "$HOME/.cargo/env"
    fish_add_path --global --move --path "$HOME/.cargo/bin"
end

# LLVM
# https://stackoverflow.com/a/42730721
fish_add_path --global --move --path '/opt/homebrew/opt/llvm/bin'

# Alias for editor
function e --wraps=emacs
    if test (count $argv) -eq 0
        emacs . &
    else
        emacs $argv
    end
end

# Check if command exists
function has_command
    if test (count $argv) -eq 0
        echo 'ERROR: has_command: no arguments given'
        return
    end

    if test (count $argv) -gt 1
        echo 'ERROR: has_command: only accepts one argument'
        return
    end

    command -v $argv[1] > /dev/null
end

# Alias for opening things
if has_command open  # macOS and some Linux
    function o --wraps=open
        open $argv
    end
else if has_command xdg-open  # Linux
    function o --wraps=xdg-open
        xdg-open $argv
    end
else if string match -q "*Windows*" (uname)  # Windows
    function o --wraps=start
        cmd.exe /c start "" $argv
    end
end

# Convenient ls aliases (see also `la` and `ll`)
abbr -a l ls
abbr -a lll la

# Use eza over ls if available (https://github.com/eza-community/eza)
if has_command eza
    function ls --wraps=eza
        eza $argv
    end

    abbr -a l1 'ls -1'
    abbr -a lsg 'ls -l --git'
end

# Use eza over exa as the latter is deprecated (https://github.com/ogham/exa/issues/1243)
if has_command eza
    function exa --wraps=eza
        eza $argv
    end
end

# Alias for `cat` command
if has_command bat
    function cat --wraps=bat
        bat $argv
    end
end

# Abbreviation for chezmoi
if has_command chezmoi
    function cz --wraps=chezmoi
        chezmoi $argv
    end
end

# Convenient alias for youtube-dl to preferred tool
if has_command yt-dlp
    function youtube-dlp --wraps=yt-dlp
        yt-dlp $argv
    end
end

# Alias for `lsusb` command
if has_command cyme
    function lsusb --wraps=cyme
        cyme --lsusb $argv
    end
end

# Use dutree over du if available (https://github.com/nachoparker/dutree)
if has_command dutree
    function du --wraps=dutree
        dutree $argv
    end

    abbr -a du1 'dutree --depth=1 --aggr=1M'
end

# Set up zoxide
if has_command zoxide
    zoxide init --cmd z fish | source

    function cd --wraps=z
        z $argv
    end

    function cdi --wraps=zi
        zi $argv
    end
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

function rgb_to_hex
    if test (count $argv) -ne 3
        echo 'ERROR: rgb_to_hex: expects exactly three arguments between 0 and 1'
        return
    end

    for f in $argv
        set -l val (math "round($f * 255)")
        printf '%02X' $val
    end
end


function _hostname
    if test (count $argv) -ne 0
        echo 'ERROR: _hostname: no arguments expected'
        return
    end

    # Can use $hostname directly
    # reddit.com/r/fishshell/comments/gqb0qm/comment/frsh1pd
    set hn $hostname

    # remove trailing .local if present
    if test (string match -r '\.local$' $hn)
        set hn (string replace -r '\.local$' '' $hn)
    end

    echo $hn
end

# Prompt adapted from:
#   github.com/jonhoo/configs/blob/cec5bd70/shell/.config/fish/config.fish#L207-L223
#
# Colours adapted from:
#   github.com/jakewilliami/tex-macros/blob/6ca86a43/macros/colours.sty
#
# Colours work well with Dracula theme:
#   git clone https://github.com/dracula/iterm.git $HOME/.config/iterm2/dracula
function fish_prompt
    # This grey is adapted from `charcoal`
    set grey (rgb_to_hex 0.447 0.489 0.517)
    # This blue was adapted from `darkelectricblue`
    set blue (rgb_to_hex 0.46 0.53 0.78)
    # This green was adapted from `darkolivegreen`
    set green (rgb_to_hex 0.50 0.68 0.40)

    # Date
    set_color --bold $grey
    echo -n "["(date "+%H:%M")"] "

    # Host
    set_color --bold $blue
    echo -n (_hostname)

    # Path
    if [ $PWD != $HOME ]
        set_color --bold $blue
        echo -n ':'
        set_color --bold yellow
        echo -n (basename $PWD)
    end

    # Git
    set_color --bold $green
    printf '%s ' (__fish_git_prompt)

    # Prompt
    set_color red
    echo -n '| '
    set_color normal
end

# Upside-down face ˙ᵕ˙
#   stackoverflow.com/a/13995944
#
# See also:
#   github.com/jonhoo/configs/blob/cec5bd70/shell/.config/fish/config.fish#L225-L311
set fish_greeting # '( .-.)'

#  ╱|、
# (˚ˎ 。7
#  |、˜〵
# じしˍ,)ノ