.PHONY: tests

tests:
	emacs -Q -batch -l ert -l tests/load-path.el -l tests/tests.el -f ert-run-tests-batch-and-exit

.PHONY: iter-tests

iter-tests:
	emacs -Q -batch -l ert -l tests/load-path.el -l tests/iter-tests.el -f ert-run-tests-batch-and-exit
