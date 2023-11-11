EMACS ?= emacs

.PHONY: tests

tests:
	$(EMACS) -Q -batch -l ert -l tests/load-path.el -l tests/tests.el -f ert-run-tests-batch-and-exit

.PHONY: iter-tests

iter-tests:
	$(EMACS) -Q -batch -l ert -l tests/load-path.el -l tests/iter-tests.el -f ert-run-tests-batch-and-exit

.PHONY: misc-tests

misc-tests:
	$(EMACS) -Q -batch -l ert -l tests/load-path.el -l tests/misc-tests.el -f ert-run-tests-batch-and-exit
