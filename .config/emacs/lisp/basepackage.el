(provide 'basepackage)

(use-package helpful)

(use-package restart-emacs)

(use-package highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'bitmap)
(setq highlight-indent-guides-responsive nil)

(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 0))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
				 ("C-x b" . counsel-ibuffer)
				 ("C-x C-f" . counsel-find-file))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Dont't start searches with ^

(use-package ivy-posframe)
;; display at `ivy-posframe-style'
;;(setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
 (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
(ivy-posframe-mode 1)

(setq ivy-posframe-parameters
      '((left-fringe . 8)
        (right-fringe . 8)))

;; Rainbow Delimeters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Jump out of couple
(use-package tab-jump-out)
(tab-jump-out-mode)

;; Google straight from emacs to browser
(use-package google-this
  :config (google-this-mode 1))

;; fancy tabs
(use-package centaur-tabs
  :hook (emacs-startup . centaur-tabs-mode)
  :init
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "M"
        centaur-tabs-cycle-scope 'tabs)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-enable-ido-completion nil)
  :config
  (centaur-tabs-mode t)
  ;; (centaur-tabs-headline-match)
  )

(setq yas-snippet-dirs
      '("~/.config/emacs/snippets"                 ;; personal snippets
        ))
(yas-global-mode 1)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

(use-package format-all)
(format-all-mode 1)

;;;; Enable Evil
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-fu)
  (setq evil-search-module 'evil-search)
  (evil-mode 1))

(use-package undo-fu) ;;Undo package

(use-package evil-surround) ;; nvim surround functionalities https://github.com/emacs-evil/evil-surround

(use-package evil-nerd-commenter) ;;Nercommeting like in i vim

(use-package evil-snipe
  :after evil
  :demand
  :config
  (evil-snipe-mode +1))

(evil-set-initial-state 'info-mode 'normal)

(use-package evil-collection)
(evil-collection-init)	


;; Displays a visual hint when editing with evil.
(use-package evil-goggles
  :after evil
  :demand
  :init
  (setq evil-goggles-duration 0.2)
  :config
  ;; (push '(evil-operator-eval
  ;;         :face evil-goggles-yank-face
  ;;         :switch evil-goggles-enable-yank
  ;;         :advice evil-goggles--generic-async-advice)
  ;;       evil-goggles--commands)
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces)
  )


;; UI 

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;;(use-package doom-themes)

;; In case that image do not show correctly run M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

(use-package marginalia)

;(use-package embark)

(require 'consult)

(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

(use-package mini-frame)

(use-package nano-theme)

;(use-package nano-emacs)
