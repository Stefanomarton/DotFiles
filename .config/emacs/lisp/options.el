(provide 'options)

;; Provide user name and email adress
(setq user-full-name "Stefano Marton"
			user-mail-address "sstefanomarton@gmail.com")

;; UI settings
(add-to-list 'default-frame-alist '(alpha 100 100)) ;; opacity settings
(setq inhibit-startup-message t) ;; don't show startup messages
(scroll-bar-mode -1) ;; no scrollbar
(tool-bar-mode -1) ;; no top bar
(tooltip-mode -1) ;; no tooltip
(menu-bar-mode -1) ;; no menu bar
(set-fringe-mode 15) ;; Set the padding of the inside windows
(setq inhibit-startup-screen t
			initial-scratch-message nil
			sentence-end-double-space nil
			ring-bell-function 'ignore
			frame-resize-pixelwise t)

;; Remove useless whitespace before saving a file
(setq-default nuke-trailing-whitespace-p t)
(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

(use-package linum-relative
	:straight t
	:init
	(add-hook 'prog-mode-hook 'linum-relative-mode)
	(add-hook 'LaTeX-mode-hook 'linum-relative-mode))

(add-hook 'prog-mode-hook 'hl-line-mode) ;; Highlight the current line
(setq hl-line-sticky-flag nil)
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 140)

(use-package centered-cursor-mode				;Add centered cursor
	:straight t
	:init
	(global-centered-cursor-mode))

(use-package all-the-icons
	:straight t
	:if (display-graphic-p))

;; Generale sane defaults
(setq inhibit-startup-buffer-menu t) ;; Don't show *Buffer list* when opening multiple files at the same time.
(setq select-enable-clipboard t) ;; System clipboard
(fset 'yes-or-no-p 'y-or-n-p) ;; No more typing the whole yes or no. Just y or n will do.

;; use common convention for indentation by default
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)

(setq vc-follow-symlinks t) ;; Follow symlinks

;; less noise when compiling elisp
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(setq native-comp-async-report-warnings-errors nil)
(setq load-prefer-newer t)

;; default to utf-8 for all the things
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8
			coding-system-for-read 'utf-8
			coding-system-for-write 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Revome useless files and keep folders clean
(setq user-emacs-directory "~/.cache/emacs")
(use-package no-littering
	:straight t)
;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
			`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq create-lockfiles nil)

(use-package ef-themes)
(use-package doom-themes)
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(load-theme 'doom-nord t)

(add-hook 'prog-mode-hook
					(lambda ()
						(whitespace-newline-mode t)))

;; Set ⏎ symbol for newlines
(setq whitespace-display-mappings
			'((newline-mark 10 [32 ?\x23CE 10])))