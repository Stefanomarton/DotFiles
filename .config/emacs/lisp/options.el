(use-package emacs
	:config
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

	;; Provide user name and email adress
	(setq user-full-name "Stefano Marton"
				user-mail-address "sstefanomarton@gmail.com")

	;; Remove useless whitespace before saving a file
	(unless (derived-mode-p 'markdown-mode)
		(setq nuke-trailing-whitespace-p t))

	;; Wrap line
	(global-visual-line-mode)

	;; Abbreviate home-dir
	(setq abbreviate-home-dir t)

	;; Display line number relative and absolute
	(setq display-line-numbers-grow-only t)
	(setq display-line-numbers-width-start 70)
	(setq display-line-numbers-type 'relative)
	(add-hook 'prog-mode-hook 'display-line-numbers-mode)

	(add-hook 'prog-mode-hook 'hl-line-mode) ;; Highlight the current line
	(setq hl-line-sticky-flag nil)

	;; Better Then Global Centered Cursor Mode
	(setq indicate-empty-lines nil)
	(setq scroll-preserve-screen-position t)
	(setq ccm-recenter-at-end-of-file t)
	(setq scroll-conservatively 1000)
	(setq scroll-margin 1000)
	(setq maximum-scroll-margin 0.5)

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
	(window-divider-mode t))

(use-package doom-themes)
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(load-theme 'doom-nord t)

(provide 'options)
