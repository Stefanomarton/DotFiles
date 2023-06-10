;;; appearance.el -*- lexical-binding: t; -*-
;;;
;;; Configuration for making Emacs look pretty.

;; Use soft wrapping instead of "scrolling" when overflowing the width of a frame.

(global-visual-line-mode t)

;; Hacking is a lot more serene when visual stimulus is kept to a minimum.

(blink-cursor-mode 0)

;; Enable `prettify-symbols' globally.

(global-prettify-symbols-mode t)

;; The menu bar can be accessed with <M-`> if I really need it, so I don't see
;; the point in having it take up the space at the top. The rest of these rarely
;; see use from me, and I'd prefer to disable them and have access to the extra
;; screen real estate.


;; Display line number relative and absolute
(setq display-line-numbers-grow-only t)
(setq display-line-numbers-width-start 70)
(setq display-line-numbers-type 'relative)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'find-file-hook 'display-line-numbers-mode)

(add-hook 'prog-mode-hook 'hl-line-mode) ;; Highlight the current line
(setq hl-line-sticky-flag nil)

;; Line spacing
(setq line-spacing 1)

(use-package nerd-icons
  :after dashboard)

(use-package frame
  :after dashboard
  :straight (:type built-in)
  :config
  ;; Make sure new frames use window-divider
  (add-hook 'before-make-frame-hook 'window-divider-mode)
  :custom
  (window-divider-default-right-width 12)
  (window-divider-default-bottom-width 1)
  (window-divider-default-places 'right-only)
  (window-divider-mode t)
  )

(add-to-list 'default-frame-alist '(alpha-background . 95))

;; A more complex, more lazy-loaded config
;; (use-package solaire-mode
;;   :if window-system ;; This is not optimal but display-graphic-p does not work
;;   :defer 1
;;   :hook
;;   ;; Ensure solaire-mode is running in all solaire-mode buffers
;;   (change-major-mode . turn-on-solaire-mode)
;;   ;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
;;   ;; itself off every time Emacs reverts the file
;;   (after-revert . turn-on-solaire-mode)
;;   ;; To enable solaire-mode unconditionally for certain modes:
;;   (ediff-prepare-buffer . solaire-mode)
;;   :custom
;;   (solaire-mode-auto-swap-bg t)
;;   :config
;;   (solaire-global-mode +1))

;; Theming

;; Suppose all custom themes are safe
(setq custom-safe-themes t)

(use-package doom-themes
  ;; :defer .5
  ;; :config
  ;; (add-to-list 'custom-theme-load-path "~/.config/emacs/themes")
  :config
  (load-theme 'doom-nord t))

;; fix color display when loading emacs in terminal
;; (defun enable-256color-term ()
;;   (interactive)
;;   (load-library "term/xterm")
;;   (terminal-init-xterm))

;; (unless (display-graphic-p)
;;   (if (string-suffix-p "256color" (getenv "TERM"))
;; 	  (enable-256color-term)))

(use-package dashboard
  :custom
  (initial-buffer-choice #'(lambda () (get-buffer-create "*dashboard*")))
  (dashboard-banner-logo-title "Welcome Back Goblin")
  ;; Content is not centered by default. To center, set
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
  (evil-define-key 'normal dashboard-mode-map
    (kbd "<leader>ff") 'find-file
    (kbd "<leader>fr") 'consult-recent-file
    (kbd "<leader>b") 'consult-buffer
    (kbd "f") 'find-file
    (kbd "h") 'consult-projectile-switch-project
    (kbd "c") (lambda ()
		        (interactive)
		        (let ((folder-path "~/.config/emacs"))
		          (find-file folder-path))))
  (dashboard-setup-startup-hook)
  )
