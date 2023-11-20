VERSION ?=
CMD ?=

SHELL := bash

.PHONY: tests
tests:
	emacs -Q -batch -l ert -l tests/load-path.el -l tests/tests.el -f ert-run-tests-batch-and-exit

.PHONY: iter-tests
iter-tests:
	emacs -Q -batch -l ert -l tests/load-path.el -l tests/iter-tests.el -f ert-run-tests-batch-and-exit

.PHONY: checkdoc
checkdoc: ## Check docstring style
	@for file in $(for_checkdoc); do \
	    echo "[checkdoc] $$file" >&2 ;\
	    emacs -Q --batch \
	        --eval "(or (fboundp 'checkdoc-file) (kill-emacs))" \
	        --eval "(checkdoc-file \"$$file\")" 2>&1 \
	        | grep . && exit 1 || true ;\
	done

.PHONY: compile
compile: ## Byte-compile
	@for file in $(for_compile); do \
	    echo "[compile] $$file" >&2 ;\
	    emacs -Q --batch -L . -f batch-byte-compile $$file 2>&1 \
	        | grep -v "^Wrote" \
	        | grep . && exit 1 || true ;\
	done
