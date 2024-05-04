# System Setup: macOS

I have been using macOS computers since 2015

https://github.com/jakewilliami/computer-info

iMac (2008), iMac (2002), MacBook (2007), Mac Mini (2009), Mbp (2015), Mbp (2016), Mbp (2022)

Do this: https://discussions.apple.com/thread/254541592?sortBy=best (now under spelling and prediciton)

ipod
iphone 3
iphone 5
iphone 8


install brew

trackpad speed high

caps -> control

dock magnify, no recent apps, smaller dock size, sleep to ?

install emacs
```
brew install --cask emacs
```

install alfred

install julia, rust

NOTE: install julia via `brew install juliaup`: https://github.com/SciML/LinearSolve.jl/issues/271#issuecomment-1511207465 (there is a [formula](https://formulae.brew.sh/formula/julia) and a [cask](https://formulae.brew.sh/cask/julia) but neither build it quite correctly (for certain cases))

Install rust via rustup: https://rustup.rs/ (see also `brew`'s `rustup-init`)

TODO: zsh

install iterm2, tmux (set up iterm2 mode)

install firefox, thunderbird, tutanota

install rectangle

install eza fd rg dutree  # Note: exa is deprecated so we use eza instead (https://github.com/ogham/exa/issues/1243)

install neofetch

configs/dotfiles

TODO: change bash as default shell

install mactex [texshop comes with it]

TODO: juliamono custom font emacs

TODO: keka/kekadefaultapp kekaexternalhelper
https://github.com/aonez/Keka/wiki/Default-application

TODO: microsoft suite

install r/rstudio
```
brew install r; brew install --cask emacs
```


brew install yt-dlp

git config --global --edit

# This is Git's per-user configuration file.
```conf
[user]
# Please adapt and uncomment the following lines:
	name = Jake Ireland
	email = jakewilliami@icloud.com
[github]
	user = jakewilliami
[ghi]
	token = !security find-internet-password -a jakewilliami -s github.com -l 'ghi token' -w
[core]
	editor = emacs
[credential]
	helper = manager
[filter "lfs"]
	clean = git-lfs clean -- %f
	smudge = git-lfs smudge -- %f
	process = git-lfs filter-process
	required = true
# TODO: get a link to resources I used for diffing word, but I remember I found that at Work
[diff "pandoc"]
    textconv = pandoc --to=markdown
    prompt = false
[alias]
    wdiff = diff --word-diff=color --unified=1
[diff "zip"]
    textconv = unzip -c -a
[diff "xlsx"]
    textconv = in2csv
# https://gist.github.com/thbar/4943276
# https://stackoverflow.com/a/19639451
# TODO: make this work in text/Emacs, similar to pandoc
[diff "pdf"]
    command = f() { diff-pdf --view "$2 $1;" } ";" f
```

can add /opt/homebrew/bin/bash to /etc/shells
stackoverflow.com/a/77052639
stackoverflow.com/a/58653886
need to restart and add to user profile defauilt login
chsh -s /opt/homebrew/bin/
.gitattributes
```
*.docx diff=pandoc
*.xlsx diff=xlsx
*.zip diff=zip
*.pdf diff=pdf
```

~/.gitconfig
```
# This is Git's per-user configuration file.
[user]
    name = Jake Ireland
    email = jakewilliami@icloud.com
[github]
    user = jakewilliami
[core]
    editor = emacs
[diff "pandoc"]
    textconv = pandoc --to=markdown
    promt = false
[alias]
    wdiff = diff --word-diff=color --unified=1
[diff "zip"]
    textconv = unzip -c- a
[diff "xlsx"]
    textconv = in2csv
[diff "pdf"]
    textconv = pdftotext
```




plex, signal, bitwarden, spotify, iina

Adding fingerprint to terminal (requires Sonoma)

Check that `/etc/pam.d/sudo` contains the following line:
```
auth       include        sudo_local
```
Your `/etc/pam.d/sudo_local` should have the line:
```
auth       sufficient     pam_tid.so
```
And you should allow this to work with iTerm2:
```
$ brew install pam-reattach
```
So now in `/etc/pam.d/sudo_local` you have *both*:
```
auth       optional       /opt/homebrew/lib/pam/pam_reattach.so
auth       sufficient     pam_tid.so
```
See also
  - https://apple.stackexchange.com/a/466029/
  - https://apple.stackexchange.com/a/355880/

Reading SICP in Emacs:
```bash
#!/bin/bash

FILE="sicp.info.gz"
APPDIR="/Applications/Emacs.app/Contents/Resources/"
TARGET="$APPDIR/info/"  # or /usr/local/share/info/ on Linux

if [ "$EUID" -ne 0 ]; then
  echo "Please run as root"
  exit 1
fi


cd /tmp/

if ! command -v install-info &> /dev/null; then
    brew install texinfo
fi

# NOTE: I had to do this in the UI because the operation wasn't permitted in terminal...
# Might
if [ ! -f "$TARGET/$FILE" ]; then
    wget "https://neilvandyke.org/sicp-texi/$FILE"
    # sudo chmod 664 "./$FILE"
    sudo mv "./$FILE" "$TARGET"
fi

# Might have to turn SIP off: https://stackoverflow.com/a/58185083/
# But a better solution is allowing your terminal emulator to have full disk access: https://stackoverflow.com/a/70236034/
# https://www.gnu.org/software/emacs/manual/html_node/efaq/Installing-Texinfo-documentation.html
install-info --info-dir="$TARGET" "$TARGET/$FILE"

# Now go to `M-x info` and find SICP.
```
