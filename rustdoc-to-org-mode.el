(require 'helm-ag)
 (defvar rustdoc-to-org-search-directory "/"
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

    (helm-ag rustdoc-to-org-search-directory (concat regex search-term))))

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
