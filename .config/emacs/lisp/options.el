(provide 'options)

;; Provide user name and email adress
(setq user-full-name "Stefano Marton"
			user-mail-address "sstefanomarton@gmail.com")

;; Remove useless whitespace before saving a file
;; (setq nuke-trailing-whitespace-p t)
;; (add-hook 'before-save-hook 'whitespace-cleanup)
;; (add-hook 'before-save-hook (lambda() (delete-trailing-whitespace)))

;; Wrap line
(add-hook 'text-mode-hook 'visual-line-mode)

;; Display line number relative and absolute
(setq display-line-numbers-grow-only t)
(setq display-line-numbers-width-start 50)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(add-hook 'prog-mode-hook 'hl-line-mode) ;; Highlight the current line
(setq hl-line-sticky-flag nil)
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 140)

;; Better Then Global Centered Cursor Mode
(setq indicate-empty-lines nil)
(setq ccm-recenter-at-end-of-file t)
(setq scroll-conservatively 1000)
(setq scroll-margin 1000)
(setq maximum-scroll-margin 0.5)

(use-package all-the-icons
	:straight t
	:if (display-graphic-p))

;; Revome useless files and keep folders clean
(setq user-emacs-directory "~/.cache/emacs")
(use-package no-littering
	:straight t)
;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
			`((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq create-lockfiles nil)

(use-package doom-themes)
(add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
(load-theme 'doom-nord t)

;; Display symbol for newline
(add-hook 'prog-mode-hook
					(lambda ()
						(whitespace-newline-mode t)))

;; Set ⏎ symbol for newlines
(setq whitespace-display-mappings
			'((newline-mark 10 [32 ?\x23CE 10])))

(use-package unicode-fonts
	:config
	(unicode-fonts-setup))

(use-package frame
	:straight (:type built-in)
	:custom
	(window-divider-default-right-width 12)
	(window-divider-default-bottom-width 1)
	(window-divider-default-places 'right-only)
	(window-divider-mode t))
;; Make sure new frames use window-divider
(add-hook 'before-make-frame-hook 'window-divider-mode)
