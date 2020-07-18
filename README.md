# emacs-rustdoc

## Org Agenda

## Setup
You can edit this command to convert std to org files, and place the result wherever you want it.
``` emacs-lisp
(dolist (file (directory-files-recursively "/home/sam/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std/" ""))
        (shell-command
         (concat "pandoc " file " -t json | stack run | pandoc -f json -o /home/sam/.emacs.d/private/rustdoc/" (file-name-sans-extension (file-name-nondirectory file)) ".org")))
```
