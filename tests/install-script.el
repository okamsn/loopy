;; -*- lexical-binding: t; -*-

(setq package-user-dir "~/.emacs.d/elpa")
(message "Current directory: %s" default-directory)
(require 'package)
(package-refresh-contents)

(message "\nInstall Loopy from dir:")
(package-install-file (expand-file-name "loopy/" default-directory))

;; `package-install-file' does not want to install these files for some reason.
(let ((place (file-name-directory (locate-library "loopy"))))
  (copy-file (expand-file-name "loopy/dir" default-directory)
             (expand-file-name "dir" place))
  (copy-file (expand-file-name "loopy/loopy.info" default-directory)
             (expand-file-name "loopy.info" place))
  (message "Installed Loopy to: %s" place)
  (cl-assert (file-exists-p (expand-file-name "loopy.info" place))
             "Info file not installed.")
  (cl-assert (file-exists-p (expand-file-name "dir" place))
             "Info file not installed."))

(message "\nInstall Loopy Dash from file:")
(package-install-file (expand-file-name "loopy-dash/loopy-dash.el" default-directory))
