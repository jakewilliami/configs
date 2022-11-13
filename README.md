<h1 align="center">Dotfiles</h1>

This subdirectory contains all of my configuration files.

These dotfiles were initially just [`.bashrc`](.bashrc) and [`.bash_profile`](.bash_profile), hence why it is under this [`bash`](..) subdirectory.  However, over time it has grown, and I should consider migrating this to a separate repository in the future.

I may have some miscellaneous notes on these configuration files below.

## Emacs

You will need to ensure you have downloaded the Emacs package `use-package`.  I use this throughout my emacs config file in order to download packages automatically, if they are not already installed..  Before this, I had to download all of the packages I use manually, and it was painful.  To ensure `use-package` is downloaded, you can run `M-x package-refresh-contents` to ensure you have the latest package index, and then run `M-x package-install RET use-package RET` to install `use-package`.  Now, when you open emacs, it will load the config file, and on your first load it will download a bunch of packages used throughout the config file.
