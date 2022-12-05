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
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
				 ("C-x b" . counsel-ibuffer)
				 ("C-x C-f" . counsel-find-file))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Dont't start searches with ^

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

(use-package doom-themes)

;; In case that image do not show correctly run M-x all-the-icons-install-fonts
(use-package all-the-icons
  :ensure t)

