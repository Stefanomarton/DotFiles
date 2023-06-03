(setq straight-check-for-modifications 'live-with-find) ;; Maybe removed in the future by straight
(add-to-list 'load-path "~/.config/emacs/lisp/")
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

;; Faster loading
(setq package-enable-at-startup nil)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(straight-use-package 'use-package) ; Base package for package configuration
(setq straight-use-package-by-default t) ;Auto install package use in the configuration

(use-package gcmh ;; Enforce a sneaky Garbage Collection strategy to minimize GC interference with user activity.
  :demand t
  :config
  (gcmh-mode 1))

;; Set custom file
(require 'options)
(require 'setup-company)
(require 'core)
(setq custom-file "~/.config/emacs/custom.el")
(load custom-file)
(put 'dired-find-alternate-file 'disabled nil)
