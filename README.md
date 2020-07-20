# emacs-rustdoc
![Demo with helm ag](https://i.imgur.com/qhTz8cW.gif)
In the demo I used <https://github.com/emacsorphanage/helm-ag> but with the following options.
``` emacs-lisp
(defun search-rustdoc ()
    (interactive)
    (let ((helm-ag-base-command "rg --smart-case --no-heading --color=never --line-number")
          (helm-ag-insert-at-point 'symbol))
      (helm-do-ag "/home/sam/.emacs.d/private/rustdoc")))

  (global-set-key (kbd "C-c C-g") 'search-rustdoc)
  ```
## Installation
* Install Pandoc https://pandoc.org/
* Install Haskell Stack https://docs.haskellstack.org/en/stable/README/
* Clone the project and run `stack install`
* Locate a rustdoc html file you wish to convert, and run `pandoc -f html <file> -t org --filter rustoc-to-org-exe -o <file>.org`

### Batch convert
You can generate all `.html` files for std by running `rustup doc`, which will put the files somewhere in `~/.rustup/`, in my case in `/home/sam/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std/)`.
You can then convert all of them with the provided `batch_convert.sh` script.
`sh batch_convert.sh "/home/sam/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std" "/home/sam/.emacs.d/private/rustdoc/"`

## TODO
* Figure out a way to get links working. All links point to .html files, they should be updated to point to .org files.
* Make code not awful and not slow.
* Fill out this list.
