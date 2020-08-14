;;; rustdoc.el --- Browse rust documentation as .org files -*- lexical-binding: t -*-

;; Copyright (c) 2020 Sam Hedin

;; Author: Sam Hedin <sam.hedin@gmail.com>
;;         Jonas Møller <jonas.moeller2@protonmail.com>
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
;; Run `M-x rustdoc-setup' to download the required files and convert the rust standard library.
;; Run `M-x rustdoc-convert-current-package' to generate and convert docs for the package you are currently visiting.

;;; Code:

(require 'helm-ag)
(require 'url)
(require 'lsp)

(if (< emacs-major-version 27)
    (defun rustdoc--xdg-data-home ()
      (or (getenv "XDG_DATA_HOME")
          (concat (file-name-as-directory (getenv "HOME"))
                  ".local/share")))
  (progn
    (require 'xdg)
    (fset 'rustdoc--xdg-data-home 'xdg-data-home)))

(defvar rustdoc-lua-filter (concat (file-name-as-directory (getenv "HOME"))
                                   ".local/bin/rustdoc-filter.lua")
  "Save location for the rustdoc lua filter.")

(defvar rustdoc-convert-prog (concat (file-name-as-directory (getenv "HOME"))
                                   ".local/bin/rustdoc-convert.sh")
  "Save location for the rustdoc conversion script.")

(defvar rustdoc-source-user "samhedin")

(defvar rustdoc-source-repo (format "https://raw.githubusercontent.com/%s/rustdoc-to-org/master/"
                                    rustdoc-source-user))

(defvar rustdoc-current-project nil
  "Location to search for documentation.
All projects and std by default, otherwise last open project and std.")

(defvar rustdoc-active-modes '(rust-mode rustic-mode)
  "Modes that rustdoc should update `rustdoc-current-project' in.")

(defvar rustdoc-save-location (concat (rustdoc--xdg-data-home) "/emacs/rustdoc"))

(defvar rustdoc-resources `((,rustdoc-convert-prog (:exec) ,(concat rustdoc-source-repo
                                                                    "convert.sh"))
                            (,rustdoc-lua-filter () ,(concat rustdoc-source-repo
                                                             "filter.lua"))))

;;;###autoload
(defun rustdoc--install-resources ()
  "Install or update the rustdoc resources."
  (dolist (resource rustdoc-resources)
    (pcase resource
      (`(,dst ,opts ,src) (condition-case nil
                              (progn
                                (url-copy-file src dst t)
                                (when (memq :exec opts)
                                  (call-process (executable-find "chmod") nil nil nil "+x" dst)))
                            (error (progn
                                     (if (file-exists-p dst)
                                         (message (format "Could not update %s, using existing one" dst))
                                       (error (format "Could not retrieve %s" dst)))))))
      (x (error "Invalid resource spec: %s" x)))))

;;;###autoload
(defun rustdoc-search (search-term)
  "Search the rust documentation for SEARCH-TERM.
Only searches in headers (structs, functions, traits, enums, etc)
to limit the number of results. "
  (interactive (list (read-string
                      (format "search term, default (%s): " (rustdoc--thing-at-point))
                      nil
                      nil
                      (rustdoc--thing-at-point))))
  (let ((helm-ag-base-command "rg -L --smart-case --no-heading --color=never --line-number")
        (search-term (concat "^\\*+ [^-]\*" (seq-reduce (lambda (acc s)
                                                    (concat acc ".*" s)) (split-string search-term " ") "")) )) ; This turns a search for `enum option' into `enum.*option', which lets there be chars between the terms
    (unless (file-directory-p rustdoc-save-location)
      (rustdoc-setup)
      (message "Running first time setup. Please re-run your search once conversion has completed.")
      (sleep-for 3))
    (unless (file-directory-p (rustdoc-current-project-doc-destination))
      (rustdoc-create-project-dir))
    (helm-ag (rustdoc-current-project-doc-destination) search-term)))

;;;###autoload
(defun rustdoc-current-project-doc-destination ()
  "The location of the documentation for the last seen project."
  (when rustdoc-current-project
      (concat rustdoc-save-location "/" (file-name-nondirectory (directory-file-name (file-name-directory (concat rustdoc-current-project "/")))))))


;;;###autoload
(defun rustdoc-create-project-dir ()
  "Create a rustdoc directory for the current project. Link with std."
    (let* ((link-tgt (concat (file-name-as-directory (rustdoc--xdg-data-home))
                            "emacs/rustdoc/std"))
           (link-name (concat (rustdoc-current-project-doc-destination) "/std"))
           (current-doc-dest (rustdoc-current-project-doc-destination)))
      (if current-doc-dest
          (progn
            (make-directory (rustdoc-current-project-doc-destination) t)
            (make-symbolic-link link-tgt link-name t))
        (message "Couldn't create project doc directory."))))


;;;###autoload
(defun rustdoc-convert-current-package ()
  "Convert the documentation for a project and its dependencies."
  (interactive)
  (unless (file-directory-p rustdoc-save-location)
    (rustdoc-setup)
    (message "Running first time setup.")
    (sleep-for 3))
  (if rustdoc-current-project
      (progn
        (message "Converting documentation for %s " rustdoc-current-project)
        (call-process "cargo" nil nil nil "makedocs")
        (let* ((docs-src (concat (file-name-as-directory rustdoc-current-project) "target/doc"))
               ;; FIXME: Many projects could share the same docs.
               ;;        *However* that would have to be versioned, so
               ;;        we'll have to figure out a way to coerce `<crate>-<version>`
               ;;        strings out of cargo, or just parse the Cargo.toml file, but
               ;;        then we'd have to review different parsing solutions.
               (finish-func (lambda (p)
                              (message (format "Finished converting docs for %s" rustdoc-current-project)))))

          (rustdoc-create-project-dir)
          (async-start-process
           "*rustdoc-convert*"
           rustdoc-convert-prog
           finish-func
           docs-src
           (rustdoc-current-project-doc-destination))))
    (message "Could not find project to convert. Visit a rust project first!")))

;;;###autoload
(defun rustdoc-setup ()
  "Setup or update rustdoc filter and convert script. Convert std."
  (interactive)
  (rustdoc--install-resources)
  (message "Setup is converting the standard library")
  (delete-directory (concat rustdoc-save-location "/std") t)
  (async-start-process
   "*rustdoc-std-conversion*"
   rustdoc-convert-prog
   (lambda (p) (message "Finished converting docs for std"))
   "std"))

;;;###autoload
(defun rustdoc--thing-at-point ()
  (interactive)
  (when lsp-mode
    (let* ((lsp-info (nth 1 (split-string (gethash "value"  (-some->> (lsp--text-document-position-params)
                                                              (lsp--make-request "textDocument/hover")
                                                              (lsp--send-request)
                                                              (lsp:hover-contents))))))
           (full-symbol-name (concat
                              (cond
                               ((string-prefix-p "core" lsp-info) (concat "std" (seq-drop lsp-info 4)))
                               ((string-prefix-p "alloc" lsp-info) (concat "std" (seq-drop lsp-info 5)))
                               (t lsp-info))
                              "::" (thing-at-point 'symbol t))))
      full-symbol-name)))

;;;###autoload
(define-minor-mode rustdoc-mode
  "Convert rust html docs to .org, and browse the converted docs."
  :lighter " browse rust documentation"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-#") 'rustdoc-search)
            map)
  (setq rustdoc-current-project (lsp-workspace-root)))

(dolist (mode '(rust-mode-hook rustic-mode-hook org-mode-hook))
  (add-hook mode 'rustdoc-mode))

(provide 'rustdoc)

;;; rustdoc.el ends here
