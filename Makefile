EMACS ?= emacs

.PHONY: tests

tests:
	$(EMACS) -Q -batch -l ert -l tests/load-path.el -l tests/tests.el -f ert-run-tests-batch-and-exit

.PHONY: iter-tests

iter-tests:
	$(EMACS) -Q -batch -l ert -l tests/load-path.el -l tests/iter-tests.el -f ert-run-tests-batch-and-exit

.PHONY: pcase-tests

pcase-tests:
	$(EMACS) -Q -batch -l ert -l tests/load-path.el -l tests/pcase-tests.el -f ert-run-tests-batch-and-exit

.PHONY: seq-tests

seq-tests:
	$(EMACS) -Q -batch -l ert -l tests/load-path.el -l tests/seq-tests.el -f ert-run-tests-batch-and-exit

.PHONY: misc-tests

misc-tests:
	$(EMACS) -Q -batch -l ert -l tests/load-path.el -l tests/misc-tests.el -f ert-run-tests-batch-and-exit

.PHONY: all-tests

all-tests:
	$(EMACS) -Q -batch \
		-l ert \
		-l tests/load-path.el \
		-l tests/tests.el \
		-l tests/pcase-tests.el \
		-l tests/seq-tests.el \
		-l tests/iter-tests.el \
		-l tests/misc-tests.el \
		-f ert-run-tests-batch-and-exit


.PHONY: info

info:
	@echo Making Info file from Texinfo file
	makeinfo --verbose --output=doc/loopy.info doc/loopy.texi
	test -f doc/loopy.info
	@echo Making Info dir file using Loopy Info file
	install-info --debug --dir-file doc/dir --info-file doc/loopy.info
	test -f doc/dir

.PHONY: tar

# Create Tar file for `package-install-file'
tar: info
	@echo Getting package version if not passed explicitly
	$(eval VERSION ?= $(shell $(EMACS) -Q -batch --eval="(progn (require 'lisp-mnt) (with-temp-buffer (insert-file-contents \"lisp/loopy.el\") (princ (or (lm-header \"package-version\") (lm-header \"version\")))))"))
	@echo Package version is $(VERSION)
	@echo Making directory to hold package files
	mkdir --verbose "loopy-$(VERSION)"
	cp --verbose --target-directory="loopy-$(VERSION)" lisp/*.el
	cp --verbose --target-directory="loopy-$(VERSION)" doc/dir doc/loopy.info
	@echo Making Tar file from that directory
	tar --create --file "loopy-$(VERSION).tar" --exclude-from=".elpaignore" --verbose "loopy-$(VERSION)"
	@echo Deleting that directory
	rm --verbose loopy-$(VERSION)/*
	rmdir --verbose loopy-$(VERSION)
