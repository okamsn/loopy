;; Add installed packages to load path.
;; Don't use Seq, as we want to load the right version.
(require 'cl-lib)
(cl-loop for i in (directory-files-recursively "~/.emacs.d/elpa/" "" t)
         when (file-directory-p i)
         do (add-to-list 'load-path i))
