(require 'ert)
(require 'elisp-demos)

(ert-deftest elisp-demos--search ()
  (should (stringp (elisp-demos--search 'mapcar)))
  (should (null (elisp-demos--search 'tom-and-jerry))))
