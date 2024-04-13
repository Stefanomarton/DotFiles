;;; core.el -*- lexical-binding: t; -*-;;;

;;; Configuration for the "core" behavior of GNU Emacs. Generally, anything
;;; which does not involve a third-party package.

;; Having Emacs open to `*scratch*' feels as though it's inviting me to punch
;; out some Lisp forms and evaluate them. I like to pretend that my computers
;; are Lisp machines.
(setq inhibit-startup-message t
      inhibit-startup-buffer-menu t
      initial-scratch-message nil
      initial-buffer-choice nil)

;; Make easy ovveride settings
(setq site-run-file nil)

;; Not a good idea
(setq max-lisp-eval-depth 10000)

;; when quiting emacs, just kill processes
(setq confirm-kill-processes nil)

;; fix clipboard in wayland
(setq select-enable-clipboard t)
(when (getenv "WAYLAND_DISPLAY")
  (setq wl-copy-p nil
        interprogram-cut-function (lambda (text)
                                    (setq-local process-connection-type 'pipe)
                                    (setq wl-copy-p (start-process "wl-copy" nil "wl-copy" "-f" "-n"))
                                    (process-send-string wl-copy-p text)
                                    (process-send-eof wl-copy-p))
        interprogram-paste-function (lambda ()
                                      (unless (and wl-copy-p (process-live-p wl-copy-p))
                                        (shell-command-to-string "wl-paste -n | tr -d '\r'")))))

;; (setq select-enable-primary t)

;; ask if local variables are safe once.
(setq enable-local-variables t)

;; Better resize
(setq frame-resize-pixelwise t)

;; Save backup files to one directory instead of making a mess of the filesystem.
(setq backup-directory-alist `(("." . "~/.cache/emacs")))

;; Revome useless files and keep folders clean
(setq user-emacs-directory "~/.cache/emacs")

;; `auto-save', in addition to the actual files it saves, maintains another file
;; listing the files it's currently taking care of.
(setq auto-save-list-file-prefix "~/.cache/emacs/auto-save-list/.saves-"
      backup-by-copying t
	  delete-old-versions t
      )

;; Disable the creation of locking symlinks in the current directory. This is a
;; very opinionated choice, and probably isn't a good idea for most.

(setq create-lockfiles nil)

;; Disable creation of backup files
(setq make-backup-files nil)

;; I don't use 'custom.el' to set variables, but a few of the packages I use do.
;; This snippet ensures that a massacre is not made of my init.el.

(setq custom-file (make-temp-file "emacs-custom-"))
(when (file-exists-p custom-file)
  (load custom-file))

;; Set default directory to home and abbreviate it
(setq default-directory "~/")

;; I care about having my history in minibuffers
(use-package savehist
  :after evil
  :init
  (savehist-mode))

;; Use a consistent confirmation dialog of "y or n".
(setq use-short-answers t)

;; Automatically revert buffers and dired listings when something on disk

(use-package autorevert
  :config
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil))

;; Always prefer newer version of a file
(setq load-prefer-newer t)

;; "Command attempted to use minibuffer while in minibuffer" gets old fast.

(setq enable-recursive-minibuffers t)

;; Select help windows when I pop them so that I can kill them with <q>.

(setq help-window-select t)

;; Most *NIX tools work best when files are terminated with a newline.

(setq require-final-newline t)

;; Sentences should be separated by a single space. Treat two sentences as such
;; when filling.

(setq sentence-end-double-space nil)

;; I use the tab key, but I generally prefer space characters to tabstops and
;; like a four-character width for indentation.

(setq-default indent-tabs-mode nil
              tab-width 4)

;; Modern conventions state that 80 characters is the standard width.

(setq fill-column 80)

;; Enable useful visual queues.

(column-number-mode t)

;; Unbind <C-z> and <C-x C-z> so that I'm not accidentally calling =suspend-frame=.

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))

;; Unbind downcase region
(global-unset-key (kbd "C-x C-l"))

;; Don't do jerky jumps when the cursor reaches the end of the window. Instead,
;; just scroll by one line.

;; Better then global centered cursor mode
(setq scroll-conservatively most-positive-fixnum
      indicate-empty-lines nil
      scroll-preserve-screen-position t
      scroll-margin 1000
      maximum-scroll-margin 0.5)

;; always follow symlinks when opening files
(setq vc-follow-symlinks t)

;; overwrite text when selected, like we expect.
(setq delete-selection-mode t)

;;Useless
(setq ring-bell-function 'ignore)

;; (setq pop-up-frames t)
;; (setq pop-up-windows nil)

;; Disable bidirectional text rendering for a modest performance boost. Just
;; need to remember to turn it on when displaying a right-to-left language!
(setq-default bidi-display-reordering 'left-to-right)

(use-package emacs
  :hook
  (after-make-frame-functions . my/new-frame-settings)
  :config
  ;; Autobyte recompile init.elc when exiting emacs
  (add-hook 'kill-emacs-hook (lambda () (byte-recompile-file user-init-file)))

  (setq server-client-instructions nil)
  (mapc
   (lambda (command)
     (put command 'disabled nil))
   '(list-timers dire-find-alternate-file narrow-to-region narrow-to-page upcase-region downcase-region))

  ;; And disable these
  (mapc
   (lambda (command)
     (put command 'disabled t))
   '(eshell project-eshell overwrite-mode iconify-frame diary))

  (defun my/new-frame-settings (frame)
    (if (daemonp)
        (setq evil-echo-state nil))))

(provide 'core)
