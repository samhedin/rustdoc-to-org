(require 'helm-ag)
(require 'url)
(defvar rustdoc-to-org-search-directory "~/.emacs.d/private/rustdoc"
  "Directory to search for converted org files")

;;;###autoload
(defun search-rustdoc (search-term)
  "Search the rust documentation for SEARCH-TERM.
Only searches in headers (structs, functions, traits, enums, etc) to limit the number of results.
Provide `prefix-arg` to only search for Level 1 headers to limit the number of search results even further. This is useful if you want to search for the name of a struct, enum or trait."
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

;;;###autoload
(defun rustdoc-to-org--install-binary ()
  "Install the rustdoc-to-org filter"
  (let ((default-directory "~/.local/bin"))
    (url-copy-file "https://github.com/samhedin/rustdoc-to-org/releases/download/v0.2/rustdoc-to-org-exe" "rustdoc-to-org-exe")
    (start-process "make_executable" nil "chmod" "+x" "rustdoc-to-org-exe")))

;;;###autoload
(defun rustdoc-to-org--convert-directory (&optional directory)
  "Convert all .html files in a directory and its subdirectories to org and place the files in `rustdoc-to-org-search-directory`"
  (interactive)

  (when (not (file-directory-p rustdoc-to-org-search-directory))
    (make-directory rustdoc-to-org-search-directory))

  (let ((default-directory "~/.local/bin/")
        (dir (if directory
                 directory
               (read-directory-name "Directory with rust html docs (for std: ~/.rustup/toolchains/<dir>/share/doc/rust/html/std): "))))

   (when (not (file-exists-p "rustdoc-to-org-exe"))
     (if (string= "yes" (read-string "Could not find rustdoc-to-org pandoc filter in your path, would you like to install it? " "yes"))
         (rustdoc-to-org--install-binary)
       (message "could not find pandoc filter, this will not work!")))

   (message "Batch converting files, this might take a while!")

  (dolist (file (directory-files-recursively dir ".html"))
    (with-temp-buffer
      (insert-file-contents file)
      (when (< 10 (count-lines (point-min) (point-max)))

        (let ((outputfile (concat rustdoc-to-org-search-directory "/" (file-name-sans-extension (file-name-nondirectory file)) ".org")))
          (start-process "convert" nil "pandoc"
                         (shell-quote-argument file)
                         "--filter"  "rustdoc-to-org-exe"
                         "-o" (shell-quote-argument outputfile))
          ))))))

(global-set-key (kbd "C-c C-s") '(lambda () (interactive) (rustdoc-to-org--convert-directory "/home/sam/.rustup/toolchains/stable-x86_64-unknown-linux-gnu/share/doc/rust/html/std/")))

;;;###autoload
(defun current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

;;;###autoload
(defun next-line-is-header-p ()
  (save-excursion
    (forward-line)
    (string-prefix-p "*"
                     (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position)))))

;;;###autoload
(defun remove-whitespace (outputfile)
    (with-temp-file outputfile
      (insert-file-contents outputfile)

      (dotimes (x 2)
        (kill-whole-line))

      (goto-char (point-min))
      (while (not (eobp))
        (when (and (current-line-empty-p) (not (next-line-is-header-p)))
                 (kill-whole-line))
          (forward-line))))

;;;###autoload
(define-minor-mode rustdoc-to-org-mode
  "Translate rust documentation to org mode, and browse it."
  :lighter " rustdoc in org"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-d") 'search-rustdoc)
            map))

;;;###autoload
(add-hook 'rustic-mode-hook 'rustdoc-to-org-mode)
;;;###autoload
(add-hook 'rust-mode-hook 'rustdoc-to-org-mode)

(provide 'rustdoc-to-org-mode)
