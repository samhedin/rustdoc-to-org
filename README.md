# emacs-rustdoc

## Installation
* Install Pandoc https://pandoc.org/
* Install Haskell Stack https://docs.haskellstack.org/en/stable/README/
* Clone the project and run `stack install`
* Locate a rustdoc html file you wish to convert, and run `pandoc <file>.html --filter rustoc-to-org-exe -o <file>.org`

I search with <https://github.com/emacsorphanage/helm-ag> but with the base command set to `rg`.
``` emacs-lisp
  (defun search-rustdoc ()
    (interactive)
    (let ((helm-ag-base-command "rg --smart-case"))
      (helm-do-ag "/home/sam/.emacs.d/private/rustdoc")))
  (global-set-key (kbd "C-c C-z") 'search-rustdoc)
  ```

### Batch convert
You can generate all `.html` files for std by running `rustup doc`, which will put the files somewhere in `~/.rustup/`.
This snippet converts all files in a directory to org, and places them in the specified directory.
``` emacs-lisp
(dolist (file (directory-files-recursively "/home/sam/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std/" ""))
        (shell-command
         (concat "pandoc " file " --filter rustdoc-to-org-exe -o /home/sam/.emacs.d/private/rustdoc/" (file-name-sans-extension (file-name-nondirectory file)) ".org")))
```
