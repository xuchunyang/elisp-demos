# Elisp API Demos

Showing an Elisp demo of `mapcar` in `C-h f mapcar`:

![Elisp Demo: mapcar](screenshot.png)

## Usage

To inject elisp demos into `*Help*`, such as `C-h f` (`M-x describe-function`), use

``` emacs-lisp
(elisp-demos-help-mode)
```

If you use [Helpful](https://github.com/Wilfred/helpful), `M-x helpful-function` etc will show the demos too.

`elisp-demos-help-mode` is a global minor mode.

## Contributing

Put your awesome Elisp demos into [elisp-demos.org](elisp-demos.org). A demo is simply an Org heading. Here is `mapcar`'s.

``` org
* mapcar

#+BEGIN_SRC elisp
(mapcar #'1+ '(1 2 3))
#+END_SRC

#+RESULTS:
: (2 3 4)
```

To keep these demos sorted alphabetically, type  `C-x h` (`M-x mark-whole-buffer`) and `C-c ^ a` (`M-x org-sort a`).
