;; -*- lexical-binding: t; -*-

(setq package-user-dir "~/.emacs.d/elpa")
(message "Current directory: %s" default-directory)
(require 'package)
(package-refresh-contents)

(message "\nInstall Loopy from tar file:")
(let ((tar-files (directory-files default-directory nil "\\`loopy-.*?.tar\\'")))
  (cl-assert (= 1 (length tar-files)))
  (package-install-file (cl-first tar-files)))

(message "\nInstall Loopy Dash from file:")
(package-install-file (expand-file-name "loopy-dash/loopy-dash.el" default-directory))
