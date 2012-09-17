EMACS=emacs

travis-ci:
	${EMACS} --version
	${EMACS} -batch -Q -l tests/run-test.el
