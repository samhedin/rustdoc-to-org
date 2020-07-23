(require 'helm-ag)
 (defvar rustdoc-to-org-search-directory "~/.emacs.d/private/rustdoc"
   "Directory to search for converted org files")

;;;###autoload
(defun search-rustdoc (search-term)
  "Search directory for SEARCH-TERM.
Provide `prefix-arg` to only search for Level 1 headers, which greatly limits the number of search results.
Useful if you want to search for the name of a Struct, Enum or Trait."
  (interactive
   (list
    (read-string (format "search term, default (%s): " (thing-at-point 'symbol))
                 nil nil (thing-at-point 'symbol))))

  (let ((helm-ag-base-command "rg  --smart-case --no-heading --color=never --line-number")
        (regex (if current-prefix-arg
                   (progn
                     (setq current-prefix-arg nil)
                     "^\\* [^-]\*")
                 "\\* [^-]\*")))

    (helm-ag (concat regex search-term) rustdoc-to-org-search-directory)))

(defun rustdoc-to-org--install-binary ()
  "Install the rustdoc-to-org filter"
  (shell-command "wget -O ~/.local/bin/rustdoc-to-org-exe  https://github.com/samhedin/rustdoc-to-org/releases/download/v0.2/rustdoc-to-org-exe &"))

;;;###autoload
(defun rustdoc-to-org--convert-directory (directory)
  (interactive)
  (when (not (file-directory-p rustdoc-to-org-search-directory))
    (make-directory rustdoc-to-org-search-directory))
  (setq async-shell-command-buffer 'new-buffer)
  (dolist (file (directory-files-recursively directory ".html"))
    (with-temp-buffer
      (insert-file-contents file)
      (when (< 10 (count-lines (point-min) (point-max)))
        (async-shell-command
         (concat "pandoc " file " --filter ~/.local/bin/rustdoc-to-org-exe -o "  rustdoc-to-org-search-directory "/" (file-name-sans-extension (file-name-nondirectory file)) ".org"))))))

(rustdoc-to-org--convert-directory "/home/sam/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std/")

;;;###autoload
(define-minor-mode rustdoc-to-org-mode
  "Translate rust documentation to org mode, and browse it."
  :lighter " rustdoc in org"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-r") 'search-rustdoc)
            map))

;;;###autoload
(add-hook 'rustic-mode-hook 'rustdoc-to-org-mode)
;;;###autoload
(add-hook 'rust-mode-hook 'rustdoc-to-org-mode)
;;;###autoload
(add-hook 'emacs-lisp-mode-hook 'rustdoc-to-org-mode)

(provide 'rustdoc-to-org-mode)
