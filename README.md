# Rustdoc to org
A Pandoc filter that converts rust documentation to .org-files, and a minor mode to go with! This is still in a very early state, but hopefully this can help someone.
![Demo with helm ag](demo.gif)

## Installation

* Install Pandoc https://pandoc.org/
* Install ripgrep https://github.com/BurntSushi/ripgrep#installation
* Install helm-ag https://github.com/bridgesense/emacs-helm-ag
* Copy `rustdoc-to-org-mode.el` and load it with `(load-file rustdoc-to-org-mode.el)`

## Usage

* Run `M-x rustdoc-to-org--convert-directory` to convert all `.html` files in a directory. The command will fetch the (very tiny) Pandoc filter from github. Batch conversion can take some time and will freeze emacs while running, so you might want to start a new emacs session while you're waiting.
* When you have converted some files, use `C-#` to run `search-rustdoc` if you are in `Rust mode`, `Rustic mode` or `Org mode`.

### Generate documentation for the rust standard library
* Generate all `.html` files for std by running `rustup doc`. In my case this puts the files in `~/.rustup/toolchains/<architecture>/share/doc/rust/html/std/)`.

### Convert documentation for a project
* Generate all `.html` files for a project by running `cargo doc`. This puts the files in `projectdir/target/doc`
* `cargo doc` tends to generate a ridiculous amount of files if you have a large project with many dependencies, many of them duplicates. You can use https://github.com/Bunogi/cargo-makedocs to limit the number of generated docs.

## TODO

* Figure out a way to get links working. All links point to .html files, they should be updated to point to .org files.
* Make code not awful and not slow.
* Fill out this list.

All suggestions, comments and more are welcomed!
