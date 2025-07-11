# Command-Line Tools

This is a file to explain commonly-used command-line tools within my workflow.

See also: [Modern UNIX](https://github.com/ibraheemdev/modern-unix).

## Essential

### [iTerm 2](https://github.com/gnachman/iTerm2)
An easy-to-use feature-full macOS terminal emulator.  I have it configured to shortcut with a translucent overlay on the window.

### [`bash`](https://git.savannah.gnu.org/cgit/bash.git)
My first "language" (I don't count LaTeX as I didn't know what I was doing), and a great scripting shell.

### [`git`](https://git.kernel.org/pub/scm/git/git.git)
If you are reading this on GitHub, then you shouldn't be suprised that `git` is likely the most essential tool in my toolkit/workflows.

### [`emacs`](https://git.savannah.gnu.org/git/emacs.git/)
Emacs is my editor of choice.  I should probably get more familiar with [`vi`](http://ex-vi.cvs.sourceforge.net/ex-vi/ex-vi/), as it's on just about every system by default, but I like the power of Emacs.

### [`tmux`](https://github.com/tmux/tmux)
Similar to [`screen`](https://git.savannah.gnu.org/git/screen.git), `tmux` is a way to manage multiple terminal sessions without having to create new tabs.  It is detachable, so is very convenient when working on many projects (which I do a lot).  Even if your terminal crashes, `tmux` will keep your sessions alive, so is terminal-independent.  Another alternative to this, which is written in Rust, is [`zellij`](https://github.com/zellij-org/zellij).

## Quality-of-Life

### [`gl`](https://github.com/jakewilliami/gl)
Instead of using git aliases, when I first began programming, I wanted to pretty-print my git log, in such a way that bolded authors that weren't me.  In that sense, [it started off relatively simple](https://github.com/jakewilliami/scripts/blob/98a327a088bc132e4418e3010b228e5f42ffff9c/bash/gl).  Since then I have extended the functionality of it, adding various git-related flags to it.  I use it often enough to have ported over to a Rust project, which then became it's own repository.

### [`fd`/`fdfind`](https://github.com/sharkdp/fd)
A fast and modern alternative to `find`, written in Rust.

### ~~[`exa`](https://github.com/ogham/exa)~~ [`eza`](https://github.com/eza-community/eza)
A fast and modern alternative to `ls`, written in Rust.

### [`rg`](https://github.com/BurntSushi/ripgrep)
A fast and modern alternative to `grep -r`, written in Rust.

### [`hyperfine`](https://github.com/sharkdp/hyperfine)
A benchmarking tool for comparing efficiency of binaries.  See references to the tool [here](./readme.md) and [here](../julia/Other/advent_of_code/2021/readme.md).

### [`bat`](https://github.com/sharkdp/bat)
An alternate version of `cat` with nice syntax highlighting, &c.  It is especially nice to pipe `diff` into: `diff f1 f2 | bat`.  Alternatively, you can download a custom `batdiff` command by running `brew install eth-p/software/bat-extras` or `pacman -S bat-extras`.  Alternatively still, you can download [`git-delta`](https://github.com/dandavison/delta) to compare files.

### [`dutree`](https://github.com/nachoparker/dutree)
A fast and modern alternative to `du`, written in Rust.

### [`duf`](https://github.com/muesli/duf)
A fast and modern alternative to `df`, written in Rust.  (See also [Canop](https://github.com/Canop)'s [`dysk`](https://github.com/Canop/dysk) [formerly [`lsf`](https://github.com/Canop/lfs)].)

### [`as-tree`](https://github.com/jez/as-tree)
Convert a list of paths (e.g., the output of `ls`) to a tree-like structure (written in Rust).  Nice for visiualising things.  (See also [`eza`]()'s `--tree` option.  Also see [Canop](https://github.com/Canop)'s [`br`](https://github.com/Canop/broot).)

### [`ebib`](https://joostkremers.github.io/ebib/ebib-manual.html)
An Emacs plugin for working with BibTeX.

### ~~[`youtube-dl`](https://github.com/ytdl-org/youtube-dl)~~ [`yt-dlp`](https://github.com/yt-dlp/yt-dlp)
Extremely useful tool for downloading videos (not just from YouTube, but that is its primary use-case).  `yt-dlp` is preferred as of February 2024 due to the following caveat:
```
==> Installing youtube-dl
==> Pouring youtube-dl--2021.12.17.arm64_sonoma.bottle.4.tar.gz
==> Caveats
The current youtube-dl version has many unresolved issues.
Upstream have not tagged a new release since 2021.

Please use yt-dlp instead.
```

### ~~[`bpytop`](https://github.com/aristocratos/bpytop)~~ [`btop`](https://github.com/aristocratos/btop)
A fast and modern replacement for something like [`htop`](https://en.wikipedia.org/wiki/Htop).

### [`inlyne`](https://github.com/Inlyne-Project/inlyne)
Locally render markdown.

## Promising

The following are some tools or applications which I have not yet adopted but look promising

### [`alacritty`](https://github.com/jwilm/alacritty)
Alacritty is a terminal emulator wriitten in Rust, and is very fast and easy to configure.  It looks really promising, and when I finally switch to Linux, with a tiling windows manager, I think it will be perfect for me.  However, as it currently stands, the reasons I do not use Alacritty are twofold:
  - I cannot get a window overlay like I can for iTerm; and
  - Alacritty [does not play nicely with the Meta key](https://github.com/alacritty/alacritty/issues/62), which is very important for Emacs, meaning I won't really be able to use Emacs with no window in Alacritty.

### [`fish`](https://github.com/fish-shell/fish-shell)
`fish` is a shell with some really nice features such as autocomplete/autosuggest, and more.  However, I am finding it hard to migrate as it is not great for scripting, I find.

### [`wtftw`](https://github.com/kintaro/wtftw)
A tiling window manager written in Rust.  A non-Rust alternative I've heard good things about is [`xmonad`](https://github.com/xmonad/xmonad).  For Wayland support, I have heard decent things about [`way-cooler`](https://github.com/way-cooler/way-cooler).

### [`polybar`](https://github.com/jaagr/polybar)
A customisable menu bar.

### [`bin`](https://github.com/marcosnils/bin)
A binary manager.

### [`cyme`](https://github.com/tuna-f1sh/cyme)
A modern alternative to [`lsusb`](https://github.com/jlhonora/lsusb).
