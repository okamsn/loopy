;; Add installed packages to load path.
;; Don't use Seq, as we want to load the right version.
(require 'cl-lib)
(let ((dir (expand-file-name (if (require 'package nil t)
                                 package-user-dir
                               "~/.emacs.d/elpa"))))
  (cl-loop for i in (directory-files-recursively dir  "" t)
           when (file-directory-p i)
           do (add-to-list 'load-path i)))
