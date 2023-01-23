;; Straight package manager bootstrappping
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(add-to-list 'load-path "~/.config/emacs/lisp/")

; Base package for package configuration
(straight-use-package 'use-package)

(require 'options)
(require 'evil)

(add-to-list 'custom-theme-load-path (expand-file-name "~/.config/emacs/themes/"))
(load-theme 'nord t)
