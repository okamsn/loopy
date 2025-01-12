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

.PHONY: doc

info:
	makeinfo --verbose --output=doc/loopy.info doc/loopy.texi
	test -f doc/loopy.info
	install-info --debug --dir-file doc/dir --info-file doc/loopy.info
	test -f doc/dir

.PHONY: tar

tar: info
	$(eval VERSION ?= $(shell $(EMACS) -Q -batch --eval="(progn (require 'lisp-mnt) (with-temp-buffer (insert-file-contents \"loopy.el\") (princ (or (lm-header \"package-version\") (lm-header \"version\")))))"))
	# While creating the archive, we have to switch to the 'doc' directory
	# while processing the Info files, otherwise `tar` will include
	# the directory structure.
	tar --create --file "loopy-$(VERSION).tar" --exclude-from=".elpaignore" --verbose loopy*.el --directory=doc dir loopy.info
