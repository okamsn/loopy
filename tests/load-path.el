;; -*- lexical-binding: t; -*-

;; Add installed packages to load path.
;; This is for testing locally, not for testing in the GitHub actions.
;; in the GH actions, we test the version of the package that we install
;; during the action (testing also the installation of the package).
;;
;; When testing locally, we want to add the current development version to the
;; load path, but we still need to add the installed dependencies to the load
;; path (though this actually adds all packages to the load path).

;; Don't use Seq, as we want to load the right version.
(require 'cl-lib)
(require 'package)

(let ((dir (expand-file-name (if (boundp 'package-user-dir)
                                 package-user-dir
                               "~/.emacs.d/elpa"))))
  (cl-loop for i in (directory-files-recursively dir  "" t)
           when (file-directory-p i)
           do (add-to-list 'load-path i)))

(add-to-list 'load-path "./lisp/.")
