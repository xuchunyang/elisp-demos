(require 'ert)
(require 'elisp-demos)

(ert-deftest elisp-demos--search ()
  (should (stringp (elisp-demos--search 'mapcar)))
  (should (null (elisp-demos--search 'tom-and-jerry))))

(ert-deftest elisp-demos--symbols ()
  "Test if the return value is sorted alphabetically."
  (should (equal (elisp-demos--symbols)
                 (sort (elisp-demos--symbols) #'string<))))

(ert-deftest elisp-demos--elisp-demos.org ()
  "elisp-demos.org should not contain any trailing whitespace."
  (should-not
   (with-temp-buffer
     (insert-file-contents elisp-demos--elisp-demos.org)
     (goto-char (point-min))
     (re-search-forward "[\s\t]+$" nil t))))

(ert-deftest elisp-demos--README.md ()
  "Make sure demos count is updated in README.md.
One can use 'make readme' to update."
  (should (equal (with-temp-buffer
                   (insert-file-contents elisp-demos--elisp-demos.org)
                   (goto-char (point-min))
                   (how-many "^\\* "))
                 (with-temp-buffer
                   (insert-file-contents
                    (expand-file-name "README.md" elisp-demos--load-dir))
                   (goto-char (point-min))
                   (and (re-search-forward "demos-\\([0-9]+\\)-blue.svg" nil t)
                        (string-to-number (match-string 1)))))))
