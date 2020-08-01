# Rustdoc to org
A Pandoc filter that converts rust documentation to .org-files, and a minor mode to go with! This is still in a very early state, but maybe it can help someone.
![Demo with helm ag](demo.gif)

## Installation

Installation is somewhat annoying at the moment, in the future I would like to make it easier. However, most of these should not be too painful to setup.

* Install Pandoc https://pandoc.org/installing.html
* Install cargo https://doc.rust-lang.org/cargo/getting-started/installation.html
* Install ripgrep with `cargo install ripgrep` or one of the alternatives: https://github.com/BurntSushi/ripgrep#installation
* Install cargo-makedocs by running `cargo install cargo-makedocs` https://github.com/Bunogi/cargo-makedocs
* Install helm-ag from MELPA with <kbd>M-x package-install [RET] helm-ag [RET]</kbd> https://github.com/bridgesense/emacs-helm-ag#installation
* Copy `rustdoc.el` and load it with `(require rustdoc.el)`

## Usage

* Run `M-x rustdoc-convert-directory` to convert all `.html` files in a directory. This will fetch the (very tiny) Pandoc filter from github. Batch conversion could take time and freeze emacs for large projects, so you might want to start a new emacs session or take a break while you're waiting.
    * Generate all `.html` files for `std` by running `rustup doc`. Now convert `~/.rustup/toolchains/<arch>/share/doc/rust/html/std/)` with `rustdoc-convert-directory`.
    * Run `M-x rustdoc-convert-current-package` to generate and convert docs for the package you are currently visiting.

* Search the org files with `rustdoc-search` (bound to `C-#` by default) if you are in `Rust mode`, `Rustic mode` or `Org mode`.


## TODO

* Figure out a way to get links working. All links point to .html files, they should be updated to point to .org files.
* Make code not awful and not slow.
* Fill out this list.

All suggestions, comments and more are welcomed!
