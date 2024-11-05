# Build

Various examples of common build scripts I use.

Note that, when compiling an executable (see [`justfile-rs`](./justfile-rs)), it is sometimes convenient to copy the binary to a directory that is in your path.  I think `/opt/local/bin/` or `$HOME/opt/bin/` is a good place for these.

Note that we prefer using [`justfile`](https://github.com/casey/just) over [`build.sh`](deprecated/).
