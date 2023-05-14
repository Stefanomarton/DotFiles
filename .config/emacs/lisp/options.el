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

(add-hook 'prog-mode-hook
					(lambda ()
						(whitespace-newline-mode t)))

;; Set ⏎ symbol for newlines
(setq whitespace-display-mappings
			'((newline-mark 10 [32 ?\x23CE 10])))

(use-package unicode-fonts
	:config
	(unicode-fonts-setup))
