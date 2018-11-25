EMACS ?= emacs

.PHONY: compile test benchmark

all: check

check: compile test

compile:
	${EMACS} -Q --batch -L . --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile elisp-demos.el

test:
	${EMACS} -Q --batch -L . -l elisp-demos-tests -f ert-run-tests-batch-and-exit

benchmark:
	${EMACS} -Q --batch  --eval "(benchmark 1 '(load-file \"elisp-demos.el\"))"
	${EMACS} -Q --batch  --eval "(benchmark 1 '(load-file \"elisp-demos.elc\"))"
