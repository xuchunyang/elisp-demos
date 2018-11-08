EMACS ?= emacs

compile:
	${EMACS} -Q --batch -L . -f batch-byte-compile elisp-demos.el
