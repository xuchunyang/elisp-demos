# Elisp API Demos

Showing an Elisp demo of `mapcar` in `C-h f mapcar`:

![Elisp Demo: mapcar](screenshot.png)

## Usage

To show the demos in `C-h f` (`M-x describe-function`), do the following

``` emacs-lisp
(advice-add 'describe-function :after #'elisp-demos--describe-function)
```

If you use [Helpful](https://github.com/Wilfred/helpful)

``` emacs-lisp
(advice-add 'helpful-update :after #'elisp-demos--helpful-update)
```

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

To keep these demos sorted, type  `C-x h` (`M-x mark-whole-buffer`) and `C-c ^ a` (`M-x org-sort a`).
