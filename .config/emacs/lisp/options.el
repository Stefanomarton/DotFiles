(use-package emacs
  :config
  (defun display-startup-echo-area-message ()
    (message 'nil))
  (setq-default line-spacing 1)
  (setq default-directory "~/"
	;; always follow symlinks when opening files
	vc-follow-symlinks t
	;; overwrite text when selected, like we expect.
	delete-selection-mode t
	;; quiet startup
	inhibit-startup-message t
	inhibit-startup-echo-area-message t
	initial-scratch-message nil
	;; hopefully all themes we install are safe
	custom-safe-themes t
	backup-by-copying t
	delete-old-versions t
	;; when quiting emacs, just kill processes
	confirm-kill-processes nil
	;; ask if local variables are safe once.
	enable-local-variables t
	;; life is too short to type yes or no
	use-short-answers t

	;; clean up dired buffers
	dired-kill-when-opening-new-dired-buffer t)

  ;; We also set the file-name-handler-alist to an empty list, and reset it after Emacs has finished initializing.
  (defvar me/-file-name-handler-alist file-name-handler-alist)
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (setq file-name-handler-alist me/-file-name-handler-alist)))

  (setq site-run-file nil)
  (setq inhibit-compacting-font-caches t)

  ;; fix color display when loading emacs in terminal
  (defun enable-256color-term ()
    (interactive)
    (load-library "term/xterm")
    (terminal-init-xterm))

  (unless (display-graphic-p)
    (if (string-suffix-p "256color" (getenv "TERM"))
	(enable-256color-term)))

  ;; Optimizations for improving I/O performance. Increase max bytes read from a sub-process in a single op (Emacs 27+)
  (when (boundp 'read-process-output-max)
    ;; 1MB in bytes, default 4096 bytes
    (setq read-process-output-max 1048576))
  (setq inhibit-startup-screen t)
  (setq	initial-scratch-message nil)
  (setq	sentence-end-double-space nil)
  (setq ring-bell-function 'ignore)
  (setq frame-resize-pixelwise t)
  (setq tab-width 4)

  ;; Enable debugging whenever we encounter an error.
  (setq debug-on-error t)

  ;; Better Then Global Centered Cursor Mode
  (setq indicate-empty-lines nil)
  (setq scroll-preserve-screen-position t)
  (setq ccm-recenter-at-end-of-file t)
  (setq scroll-conservatively 101)
  (setq scroll-margin 1000)
  (setq maximum-scroll-margin 0.5)
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

  ;; less noise when compiling elisp
  (setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
  (setq native-comp-async-report-warnings-errors nil)
  (setq load-prefer-newer t)
  (setq make-backup-files nil
	lock-files nil)
  (setq vc-follow-symlinks t) ;; Follow symlinks
  ;; UI settings
  ;; (add-to-list 'default-frame-alist '(alpha 100 100)) ;; opacity settings
  (scroll-bar-mode -1) ;; no scrollbar
  (tool-bar-mode -1) ;; no top bar
  (tooltip-mode -1) ;; no tooltip
  (menu-bar-mode -1) ;; no menu bar

  ;; use common convention for indentation by default
  ;; (setq indent-tabs-mode t)

  ;; Set default font
  (set-face-attribute 'default nil
		      :family "JetBrains Mono Nerd Font"
		      :height 120
		      :weight 'normal
		      :width 'normal)

  ;; Italic comments
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)
  (set-face-attribute 'font-lock-function-name-face nil :slant 'italic)
  (set-face-attribute 'font-lock-variable-name-face nil :slant 'italic)

  (fset 'yes-or-no-p 'y-or-n-p) ;; No more typing the whole yes or no. Just y or n will do.


  ;; Remove useless whitespace before saving a file
  (unless (derived-mode-p 'markdown-mode)
    (setq nuke-trailing-whitespace-p t))

  ;; Wrap line
  (global-visual-line-mode)

  ;; Abbreviate home-dir
  (setq abbreviate-home-dir t)

  ;; Hacky
  (setq max-lisp-eval-depth 10000)

  ;; Display line number relative and absolute
  (setq display-line-numbers-grow-only t)
  (setq display-line-numbers-width-start 70)
  (setq display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)

  (add-hook 'prog-mode-hook 'hl-line-mode) ;; Highlight the current line
  (setq hl-line-sticky-flag nil)

  ;; Revome useless files and keep folders clean
  (setq user-emacs-directory "~/.cache/emacs")

  (use-package no-littering
    :straight t)


  ;; no-littering doesn't set this by default so we must place
  ;; auto save files in the same path as it uses for sessions
  (setq auto-save-file-name-transforms
	`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
  (setq create-lockfiles nil)

  ;; Make sure new frames use window-divider
  (add-hook 'before-make-frame-hook 'window-divider-mode)

  ;; ;; Display symbol for newline
  ;; (add-hook 'prog-mode-hook
  ;; 					(lambda ()
  ;; 						(whitespace-newline-mode t)))

  ;; ;; Set ⏎ symbol for newlines
  ;; (setq whitespace-display-mappings
  ;; 			'((newline-mark 10 [32 ?\x23CE 10])))

  (add-hook 'before-save-hook
	    (lambda ()
	      (unless (derived-mode-p 'markdown-mode)
		(lambda() (delete-trailing-whitespace))
		)))

  (add-hook 'before-save-hook
	    (lambda ()
	      (unless (derived-mode-p 'markdown-mode)
		'whitespace-cleanup
		)))

  )

(use-package nerd-icons
  :straight t)

(use-package frame
  :straight (:type built-in)
  :custom
  (window-divider-default-right-width 12)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'right-only)
  (window-divider-mode t)
  )

(use-package doom-themes
  :defer t
  ;; :config
  ;; (add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
  :init
  (load-theme 'doom-nord t))

(provide 'options)
