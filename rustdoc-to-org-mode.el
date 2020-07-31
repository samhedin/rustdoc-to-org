;; -*- lexical-binding: t -*-
(require 'helm-ag)
(require 'url)
(defvar rto-search-directory "~/.emacs.d/private/rustdoc"
  "Directory to search for converted org files")
(defvar rto-lua-filter-location "~/.local/bin/filter.lua"
  "Default save location for the rustdoc-to-org lua filter")

;;;###autoload
(defun rto-get-filter ()
  "Install or update the rustdoc-to-org filter"
  (url-copy-file "https://raw.githubusercontent.com/samhedin/rustdoc-to-org/master/filter.lua"
                 rto-lua-filter-location t))

;;;###autoload
(defun search-rustdoc (search-term)
  "Search the rust documentation for SEARCH-TERM.
Only searches in headers (structs, functions, traits, enums, etc) to limit the number of results.
Provide `prefix-arg` to only search for Level 1 headers to limit the number of search results even further. This is useful if you want to search for the name of a struct, enum or trait."
  (interactive (list (read-string (format "search term, default (%s): "
                                          (thing-at-point 'symbol))
                                  nil
                                  nil
                                  (thing-at-point 'symbol))))
  (let ((helm-ag-base-command "rg  --smart-case --no-heading --color=never --line-number")
        (regex (if current-prefix-arg
                   (progn
                     (setq current-prefix-arg nil)
                     "^\\* [^-]\*")
                 "\\* [^-]\*")))
    (helm-ag rto-search-directory
             (concat regex search-term))))

;;;###autoload
(defun rto-convert-directory (&optional directory)
  "Convert all .html files in DIRECTORY and its subdirectories to org and place the files in `rto-search-directory`
If DIRECTORY is not given, prompts user to select directory."
  (interactive)
  (when (not (file-directory-p rto-search-directory))
    (make-directory rto-search-directory))
  (let ((default-directory "~/.local/bin/")
        (dir (if directory
                 directory
               (read-directory-name "Directory with rust html docs (for std: ~/.rustup/toolchains/<dir>/share/doc/rust/html/std): "))))
    (rto-get-filter)
    (dolist (file (directory-files-recursively dir ".html"))
      (if (with-temp-buffer
            (insert-file-contents file)
            (< 10 (count-lines (point-min)
                               (point-max))))
          (progn
            (message "converting %s" file)
            (rto-convert-file dir file))

        (message "Ignoring conversion of %s as it is probably a redirect"
                 file)))

    (message "Batch conversion done!")))

;;;###autoload
(defun rto-convert-file (dir file)
  (let* ((outputfile (concat rto-search-directory
                             "/"
                             (file-name-sans-extension (file-relative-name file dir))
                             ".org")))
    (make-directory (file-name-directory outputfile)
                    t)
    (call-process "pandoc"
                  nil
                  "*pandoc-log*"
                  nil
                  (shell-quote-argument file)
                  "--lua-filter"
                  (file-truename rto-lua-filter-location)
                  "-o"
                  (shell-quote-argument outputfile))
    (rto-remove-whitespace outputfile)))

;;;###autoload
(defun rto-remove-whitespace (outputfile)
  (condition-case nil
      (with-temp-file outputfile
        (insert-file-contents outputfile)
        (goto-char (point-min))
        (while (not (eobp))
          (when (and (rto-current-line-empty-p)
                     (not (rto-next-line-is-header-p))) ;; Delete newlines unless the next line is a header.
            (kill-whole-line))
          (forward-line)))
    (error (message "Missing %s " outputfile))))


;;;###autoload
(defun rto-parent-directory (dir)
  (unless (equal "/" dir)
    (file-name-directory (directory-file-name dir))))

;;;###autoload
(defun rto-find-doc-dir-helper (current-dir)
  "Search for a file named FNAME upwards through the directory hierarchy, starting from CURRENT-DIR"
  (let* ((fname "target")
         (file (concat current-dir fname))
        (parent (rto-parent-directory (expand-file-name current-dir))))
    (if (file-exists-p file)
        (concat file "/" "doc")
      (when parent
        (rto-find-doc-dir-helper parent)))))

;;;###autoload
(defun rto-find-doc-dir ()
  (rto-find-doc-dir-helper (file-name-directory (buffer-file-name))))

;;;###autoload
(defun rto-convert-current-package ()
  (interactive)
  (call-process "cargo" nil "*cargo-makedoc*" nil "makedocs")
  (let ((dir (rto-find-doc-dir)))
    (rto-convert-directory dir)))

;;;###autoload
(defun rto-current-line-empty-p ()
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

;;;###autoload
(defun rto-next-line-is-header-p ()
  (save-excursion
    (forward-line)
    (string-prefix-p "*"
                     (buffer-substring-no-properties (line-beginning-position)
                                                     (line-end-position)))))


;;;###autoload
(define-minor-mode rustdoc-to-org-mode
  "Lets you convert rust html docs to .org, and search in the org files."
  :lighter " rustdoc in org"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-#") 'search-rustdoc)
            map))

(add-hook 'rustic-mode-hook 'rustdoc-to-org-mode)
(add-hook 'rust-mode-hook 'rustdoc-to-org-mode)
(add-hook 'org-mode-hook 'rustdoc-to-org-mode)

(provide 'rustdoc-to-org-mode)
