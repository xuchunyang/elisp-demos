(require 'ert)
(require 'elisp-demos)

(ert-deftest elisp-demos--search ()
  (should (stringp (elisp-demos--search 'mapcar)))
  (should (null (elisp-demos--search 'tom-and-jerry))))

(ert-deftest elisp-demos--symbols ()
  "Test if the return value is sorted alphabetically."
  (should (equal (elisp-demos--symbols)
                 (sort (elisp-demos--symbols) #'string<))))

(ert-deftest elisp-demos-no-trailing-whitespace ()
  "elisp-demos.org should not contain any trailing whitespace."
  (should-not
   (with-temp-buffer
     (insert-file-contents elisp-demos--elisp-demos.org)
     (goto-char (point-min))
     (re-search-forward "[\s\t]+$" nil t))))
