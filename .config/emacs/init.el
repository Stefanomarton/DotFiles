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

(straight-use-package 'use-package) ; Base package for package configuration
(setq straight-use-package-by-default t) ;Auto install package use in the configuration

(require 'options)
(require 'setup-company)
(require 'core)

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)
(put 'dired-find-alternate-file 'disabled nil)
