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
;; Run ./convert.sh to download and convert all docs to org.
;; Run `M-x rustdoc-convert-current-package' to generate and convert docs for the package you are currently visiting.
;; You can customize the directory to store and search for org docs by editing `rustdoc-search-directory'.

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


(defvar rustdoc-local-directory ".rustdoc"
  "Directory to search for converted org files.")

(defvar rustdoc-lua-filter (concat (file-name-as-directory (getenv "HOME"))
                                   ".local/bin/rustdoc-filter.lua")
  "Save location for the rustdoc lua filter.")

(defvar rustdoc-convert-prog (concat (file-name-as-directory (getenv "HOME"))
                                   ".local/bin/rustdoc-convert.sh")
  "Save location for the rustdoc conversion script.")

(defvar rustdoc-source-user "samhedin")

(defvar rustdoc-source-repo (format "https://raw.githubusercontent.com/%s/rustdoc-to-org/master/"
                                    rustdoc-source-user))

(defvar rustdoc-current-project (concat (file-name-as-directory (rustdoc--xdg-data-home))
                                                "emacs/rustdoc/std")
  "Location to search for documentation. Search std by default, then last open project.")

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
to limit the number of results.
Provide a raw prefix arg to only search for Level 1 headers,
this limits the number of search results even further.
This is useful if you want to search for the name of a struct, enum or trait."
  (interactive (list (read-string
                      (format "search term, default (%s): " (thing-at-point 'symbol))
                      nil
                      nil
                      (thing-at-point 'symbol))))
  (let ((helm-ag-base-command "rg -L --smart-case --no-heading --color=never --line-number")
        (regex (if current-prefix-arg
                   (progn
                     (setq current-prefix-arg nil)
                     "^\\* [^-]\*")
                 "\\* [^-]\*")))
    (unless (file-directory-p rustdoc-save-location)
      (rustdoc-setup)
      (error "Setting up rustdoc for the first time. Please run your search again when you see the message Finished converting...")) ; FIXME: If the user has not run rustdoc-setup yet, we do it for them.
    ; Since this is async this will immediately continue and result in a failed search. Once the conversion finishes search will work.
    (unless (file-directory-p (rustdoc-current-project-doc-destination))
      (rustdoc-create-project-dir))
    (helm-ag (rustdoc-current-project-doc-destination) (concat regex search-term))))

(defun rustdoc-current-project-doc-destination ()
  "The location of the documentation for the last seen project."
  (concat rustdoc-save-location "/" (file-name-nondirectory (directory-file-name (file-name-directory (concat rustdoc-current-project "/"))))))

;;;###autoload
(defun rustdoc-create-project-dir ()
  "Create a rustdoc directory for the current project. Link with std."
  (make-directory (rustdoc-current-project-doc-destination) t)
    (let* ((link-tgt (concat (file-name-as-directory (rustdoc--xdg-data-home))
                            "emacs/rustdoc/std"))
          (link-name (concat (rustdoc-current-project-doc-destination) "/std")))
      (make-symbolic-link link-tgt link-name t)))


;;;###autoload
(defun rustdoc-convert-current-package ()
  "Convert the documentation for a project and its dependencies."
  (interactive)

  (message "Converting documentation for %s " rustdoc-current-project)
  (call-process "cargo" nil nil nil "makedocs")
  (let* ((docs-src (concat (file-name-as-directory rustdoc-current-project) "target/doc"))
         ;; FIXME: Currently, the converted files are stored inside the project.
         ;;        I want to store them elsewhere, as many projects could share
         ;;        the same docs. *However* that would have to be versioned, so
         ;;        we'll have to figure out a way to coerce `<crate>-<version>`
         ;;        strings out of cargo, or just parse the Cargo.toml file, but
         ;;        then we'd have to review different parsing solutions.
         ;;
         ;;        For now, I think this is an ok solution.
         ;; Update: Converted files are now stored in a directory next to where std is stored.
         (finish-func (lambda (p)
                        (message (format "Finished converting docs for %s" rustdoc-current-project)))))

    (rustdoc-create-project-dir)
    (async-start-process
     "*rustdoc-convert*"
     rustdoc-convert-prog
     finish-func
     docs-src
     (rustdoc-current-project-doc-destination))))




;;;###autoload
(defun rustdoc-setup ()
  "First-time setup for rustdoc."
  (interactive)
  (rustdoc--install-resources)
  (message "Setup is converting the standard library")
  (async-start-process
   "*rustdoc-std-conversion*"
   rustdoc-convert-prog
   (lambda (p)
     (message "Finished converting docs for std"))
   "std"))


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
