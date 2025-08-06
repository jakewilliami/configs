<h1 align="center">Configs</h1>

The [`src`](./src/) subdirectory contains all of my configuration files.

These dotfiles were initially just [`.bashrc`](./src/dotfiles/dot_bashrc) and [`.bash_profile`](./src/dotfiles/dot_bash_profile).  However, over time it has grown, and I should consider migrating this to a separate repository in the future.

This repository also contains my configuration files I use throughout my projects, such as how formatters/linters should work ([`format/`](./src/format/)), and reasonable defaults for CI/CD jobs ([`ci/`](./src/ci/)) and build scripts ([`build/`](./src/build/)).

Importantly, since [#14](https://github.com/jakewilliami/configs/pull/14), the [`dotfiles/`](./src/dotfiles/) directory is managed by [Chezmoi](https://www.chezmoi.io/).  To instantiate these dotfiles on a new machine, you can run
```shell
chezmoi init https://github.com/jakewilliami/configs.git
```

To see what changes can be applied to your home directory, run
```shell
chezmoi apply -nv
```

These options' long forms are `--dry-run` and `--verbose` respectively.

For convenience, I have added a symbolic link to the Chezmoi where I normally keep my config files:
```shell
ln -s ~/.local/share/chezmoi/ ~/projects/configs
```

When you first log onto a machine, it's good to pull any remote changes first, before applying
```shell
chezmoi git pull
```

If you have local changes that aren't up to date with the dotfiles/configs repo, you can run
```shell
chezmoi re-add -nv
```

To see what you can change locally.  Otherwise, when you run `chezmoi apply` you will be asked how you want to handle the local changes, and there [is currently](https://github.com/twpayne/chezmoi/issues/2162) no option to accept changes.

I have an alias for `chezmoi` set as `cz` for convenience.

See [Chezmoi quick start](https://www.chezmoi.io/quick-start/).

---

## Emacs

You will need to ensure you have downloaded the Emacs package `use-package`.  I use this throughout my emacs config file in order to download packages automatically, if they are not already installed (see [#7](https://github.com/jakewilliami/configs/issues/7))..  Before this, I had to download all of the packages I use manually, and it was painful.  To ensure `use-package` is downloaded, you can run `M-x package-refresh-contents` to ensure you have the latest package index, and then run `M-x package-install RET use-package RET` to install `use-package`.  Now, when you open emacs, it will load the config file, and on your first load it will download a bunch of packages used throughout the config file.

## CLI Tools

See [notes on useful CLI tools](/.command-line-tools.md).

## System Setup

### macOS

See [rough notes on system setup for macOS](./system-setup-macos.md).
