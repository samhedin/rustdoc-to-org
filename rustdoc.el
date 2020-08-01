;;; rustdoc.el --- Browse rust documentation as .org files -*- lexical-binding: t -*-

;; Copyright (c) 2020 Sam Hedin

;; Author: Sam Hedin <sam.hedin@gmail.com>
;; URL: https://github.com/samhedin/rustdoc
;; Version: 0.5
;; Keywords: docs languages
;; Package-Requires: ((emacs "25.1") (helm-ag "0.62"))

;; This file is NOT part of GNU Emacs.

;; MIT License

;; Copyright (c) 2020 Sam Hedin

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; This package lets you convert rustdoc html-files to org mode files, and lets you browse them with `rustdoc-search'.
;; Run `rustdoc-convert-directory' to convert all `.html` files in a directory. This will fetch the (very tiny) Pandoc filter from github.
;; Batch conversion could take time and freeze Emacs for large projects, so you might want to start a new Emacs session or take a break while you're waiting.
;; Generate all `html' files for `std' by running `rustup doc'. Now convert `~/.rustup/toolchains/<arch>/share/doc/rust/html/std/)' with `rustdoc-convert-directory'.
;; Run `M-x rustdoc-convert-current-package' to generate and convert docs for the package you are currently visiting.
;; You can customize the directory to store and search for org docs by editing `rustdoc-search-directory'.
;; You can customize the location the lua filter is saved in by editing `rustdoc-lua-filter'

;;; Code:

(require 'helm-ag)
(require 'url)

(defvar rustdoc-search-directory (concat user-emacs-directory "private/rustdoc")
  "Directory to search for converted org files.")

(defvar rustdoc-lua-filter "~/.local/bin/filter.lua"
  "Default save location for the rustdoc lua filter.")

(defun rustdoc--get-filter ()
  "Install or update the rustdoc filter."
  (condition-case nil
      (url-copy-file "https://raw.githubusercontent.com/samhedin/rustdoc-to-org/master/filter.lua" rustdoc-lua-filter t)
    (error (progn
             (if (file-exists-p rustdoc-lua-filter)
                 (message "Couldn't update pandoc filter, using existing one.")
               (error "Could not retrieve pandoc filter"))))))

;;;###autoload
(defun rustdoc-search (search-term)
  "Search the rust documentation for SEARCH-TERM.
Only searches in headers (structs, functions, traits, enums, etc)
to limit the number of results.
Provide a raw prefix arg to only search for Level 1 headers,
this limits the number of search results even further.
This is useful if you want to search for the name of a struct, enum or trait."
  (interactive (list (read-string
                      (format "search term, default (%s): " (thing-at-point 'symbol))
                      nil
                      nil
                      (thing-at-point 'symbol))))
  (let ((helm-ag-base-command "rg  --smart-case --no-heading --color=never --line-number")
        (regex (if current-prefix-arg
                   (progn
                     (setq current-prefix-arg nil)
                     "^\\* [^-]\*")
                 "\\* [^-]\*")))
    (helm-ag rustdoc-search-directory (concat regex search-term))))

;;;###autoload
(defun rustdoc-convert-directory (&optional directory)
  "Convert all .html files in DIRECTORY and its subdirectories to .org.
Place the files in `rustdoc-search-directory`
If DIRECTORY is not given, prompts user to select directory."
  (interactive)
  (make-directory rustdoc-search-directory t)
  (let ((dir (if directory
                 directory
               (read-directory-name "Directory with rust html docs (for std: ~/.rustup/toolchains/<arch>/share/doc/rust/html/std): "))))
    (rustdoc--get-filter)
    (dolist (file (directory-files-recursively dir ".html"))
      (if (with-temp-buffer
            (insert-file-contents file)
            (< 10 (count-lines (point-min)
                               (point-max))))
          (progn
            (message "converting %s" file)
            (rustdoc-convert-file dir file))
        (message "Ignoring conversion of %s as it is probably a redirect"
                 file)))
    (message "Batch conversion done!")))

;;;###autoload
(defun rustdoc-convert-file (dir file)
  "Convert a html FILE to org.
Place the output in `rustdoc-search-directory', saving its relative path thanks to DIR."
  (let* ((outputfile (concat rustdoc-search-directory
                             "/"
                             (file-name-sans-extension (file-relative-name file dir))
                             ".org")))
    (make-directory
     (file-name-directory outputfile)
     t)
    (call-process "pandoc"
                  nil
                  "*pandoc-log*"
                  nil
                  (file-truename file)
                  "--lua-filter"
                  (file-truename rustdoc-lua-filter)
                  "-o"
                  (file-truename outputfile))
    (rustdoc-remove-whitespace outputfile)))


;;;###autoload
(defun rustdoc-remove-whitespace (outputfile)
  "Remove blank lines from OUTPUTFILE, unless the line after is a header."
  (condition-case nil
      (with-temp-file outputfile
        (insert-file-contents outputfile)
        (goto-char (point-min))
        (while (not (eobp))
          (when (and (rustdoc--current-line-empty-p)
                     (not (rustdoc--next-line-is-header-p)))
            (kill-whole-line))
          (forward-line)))
    (error (message "Missing %s " outputfile))))


;;;###autoload
(defun rustdoc-convert-current-package ()
  "Generate and convert the documentation for the current rust package."
  (interactive)
  (call-process "cargo" nil "*cargo-makedoc*" nil "makedocs")
  (rustdoc-convert-directory
   (concat
    (locate-dominating-file (buffer-file-name) "target")
    "target/doc")))

;; from https://emacs.stackexchange.com/questions/16792/easiest-way-to-check-if-current-line-is-empty-ignoring-whitespace/16793#16793
(defun rustdoc--current-line-empty-p ()
  "Return whether the current line is empty or not."
  (save-excursion
    (beginning-of-line)
    (looking-at-p "[[:space:]]*$")))

(defun rustdoc--next-line-is-header-p ()
  "Return whether the next line is an org mode header or not."
  (save-excursion
    (forward-line)
    (string-prefix-p
     "*"
     (buffer-substring-no-properties
      (line-beginning-position)
      (line-end-position)))))


;;;###autoload
(define-minor-mode rustdoc-mode
  "Lets you convert rust html docs to .org, and search the converted org files."
  :lighter " rustdoc in org"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-#") 'rustdoc-search)
            map))

(dolist (mode '(rust-mode-hook rustic-mode-hook org-mode-hook))
  (add-hook mode 'rustdoc-mode))

(provide 'rustdoc)

;;; rustdoc.el ends here
