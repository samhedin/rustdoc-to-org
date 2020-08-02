;;; rustdoc-to-org-test.el --- Tests for rustdoc-to-org -*- lexical-binding: t -*-

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

;; Tests for rustdoc

;;; Code:

(require 'rustdoc (file-truename "rustdoc.el"))


(ert-deftest rustdoc-get-filter-test ()
  (rustdoc--get-filter))

(ert-deftest rustdoc-convert-basic-file ()
  (let ((rustdoc-search-directory default-directory)
        (inputfile (file-truename "debug_files/enum.Option.html"))
        (outputfile (file-truename "debug_files/enum.Option.org")))

    (rustdoc-convert-file (file-truename ".") inputfile)
    (should (file-exists-p "debug_files/enum.Option.org"))
    (delete-file outputfile)))

(ert-deftest rustdoc-convert-directory-test ()
  (let* ((outputdir (concat default-directory  "debug_files/batch_output"))
        (rustdoc-search-directory outputdir)
      (inputdir (file-truename "debug_files")))
    (rustdoc-convert-directory inputdir)
    (should (< 2 (length (directory-files outputdir))))
    (delete-directory outputdir t))) ; Remove this cleanup line if you need to debug


;;; rustdoc-to-org-test.el ends here
