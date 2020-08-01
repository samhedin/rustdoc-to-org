;;; rustdoc-to-org-test.el --- Tests for rustdoc-to-org
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

;;; rustdoc-to-org-test.el ends here