;;; appearance.el -*- lexical-binding: t; -*-
;;;
;;; Configuration for making Emacs look pretty.

(use-package emacs
  :config
  ;; Prefere visual line
  (global-visual-line-mode t)

  ;; I keep losing the curson
  (blink-cursor-mode 1)
  ;; Enable `prettify-symbols' globally.
  (global-prettify-symbols-mode t)

  ;; Display line number relative and absolute
  (setq display-line-numbers-grow-only t)
  (setq display-line-numbers-width-start 70)
  (setq display-line-numbers-type 'relative)
  (add-hook 'prog-mode-hook 'display-line-numbers-mode)
  (add-hook 'find-file-hook 'display-line-numbers-mode)

  ;; Highlight the current line
  (add-hook 'prog-mode-hook 'hl-line-mode)
  (setq hl-line-sticky-flag nil)          ; Avoid seeing the bar in all windows

  ;; Line spacing
  (setq line-spacing 1)
  )

;; I like icons
(use-package nerd-icons
  :after dashboard)

;; Customize the divider beetween windows
;; (use-package frame
;;   :after dashboard
;;   :straight (:type built-in)
;;   :config
;;   ;; Make sure new frames use window-divider
;;   (add-hook 'before-make-frame-hook 'window-divider-mode)
;;   :custom
;;   (window-divider-default-right-width 3)
;;   (window-divider-default-bottom-width 3)
;;   (window-divider-default-places t)
;;   )

(use-package spacious-padding
  :after dashboard
  :config
  ;; These are the defaults, but I keep it here for visiibility.
  (setq spacious-padding-subtle-mode-line t)
  (setq spacious-padding-widths
        '( :internal-border-width 0
           :header-line-width 4
           :mode-line-width 1
           :tab-width 5
           :right-divider-width 0
           :scroll-bar-width 0))
  (spacious-padding-mode 1))

;; Theming

;; (use-package doom-themes
;;   ;; :defer .5
;;   ;; :config
;;   ;; (add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
;;   :config
;;   (load-theme 'doom-nord t))

(use-package ewal
  :init
  (setq ewal-use-built-in-always-p nil
        ewal-use-built-in-on-failure-p t
        ewal-built-in-palette "sexy-material")
  :config
  ;; Suppose all custom themes are safe
  (setq ewal-shade-percent-difference 10)
  (setq custom-safe-themes t)
  (add-to-list 'custom-theme-load-path "~/.config/emacs/")
  (load-theme 'pywal)
  )

;; Cool dashboard setting with minimal loading times
(use-package dashboard
  :if (< (length command-line-args) 2)
  :custom
  (initial-buffer-choice #'(lambda () (get-buffer-create "*dashboard*")))
  (dashboard-banner-logo-title "Welcome Back Goblin")
  (dashboard-startup-banner "~/.config/emacs/themes/logo.txt")
  (dashboard-center-content t)
  (dashboard-set-footer nil)
  (dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
  ;; (setq dashboard-set-navigator t)
  ;; (setq dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items nil)
  ;; (dashboard-items '((recents  . 10)))
  ;; (bookmarks . 5))
  ;; (projects . 5)))
  ;; (agenda . 5)
  ;; (registers . 5)))
  :config
  ;; define custom mode map avoiding problems with lazyloading
  (evil-define-key 'normal dashboard-mode-map
    (kbd "<leader>ff") 'find-file
    (kbd "<leader>fr") 'consult-recent-file
    (kbd "<leader>b") 'consult-buffer
    (kbd "f") 'find-file
    (kbd "<leader>pp") 'projectile-switch-project
    (kbd "c") (lambda ()
		        (interactive)
		        (let ((folder-path "~/.config/emacs"))
		          (find-file folder-path))))
  (dashboard-setup-startup-hook)
  )

(provide 'appearance)
