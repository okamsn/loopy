;; -*- lexical-binding: t; -*-

(setq package-user-dir "~/.emacs.d/elpa")
(message "Current directory: %s" default-directory)
(require 'package)
(package-refresh-contents)

;; NOTE: This definition is needed for install Dash for `loopy-dash' for some
;; reason:

;;;###autoload
(unless (fboundp 'lisp-data-mode)
;;;###autoload
  (define-derived-mode lisp-data-mode prog-mode "Lisp-Data"
    "Major mode for buffers holding data written in Lisp syntax."
    :group 'lisp
    (lisp-mode-variables nil t nil)
    (setq-local electric-quote-string t)
    (setq imenu-case-fold-search nil)))

(message "\nInstall Loopy from tar file:")
(let ((tar-files (directory-files default-directory nil "\\`loopy-.*?.tar\\'")))
  (cl-assert (= 1 (length tar-files)))
  (package-install-file (cl-first tar-files)))

(message "\nInstall Loopy Dash from file:")
(package-install-file (expand-file-name "loopy-dash/loopy-dash.el" default-directory))
