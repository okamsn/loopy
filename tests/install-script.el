(setq package-user-dir "~/.emacs.d/elpa")
(message "Current directory: %s" default-directory)
(require 'package)
;; (package-install 'seq)
;; (package-install 'map)
;; (package-install 'stream)
;; (package-install 'compat)
(dolist (i '(("gnu" . "https://elpa.gnu.org/packages/")
             ("gnu-devel" . "https://elpa.gnu.org/devel/")
             ("nongnu" . "https://elpa.nongnu.org/nongnu/")
             ("nongnu-devel" . "https://elpa.nongnu.org/nongnu-devel/")))
  (add-to-list 'package-archives i))

(dolist (i '(("gnu-devel" . -1)
             ("nongnu-devel" . -1)))
  (add-to-list 'package-archive-priorities i))

(package-refresh-contents)

(package-install
 (car (seq-filter (lambda (x)
                    (equal (package-desc-archive x) "gnu-devel"))
                  (alist-get 'stream package-archive-contents))))

(message "\nInstall from dir:")
(package-install-file "~/work/loopy/loopy/test-install")
(message "\nInstall from file:")
(package-install-file "./loopy-dash.el")
