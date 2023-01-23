(provide 'evil)

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  (setq evil-search-module 'evil-search)
  :config
  (evil-mode 1))

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-goggles
  :straight t
	:after evil
  :config
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces))

(use-package undo-fu
  :straight t
						 )

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

(use-package evil-snipe
  :straight t
	:config
   (evil-snipe-mode 1)
   (evil-snipe-override-mode 1)
	 (setq evil-snipe-scope 'visible)

(evil-define-key '(normal motion) evil-snipe-local-mode-map
  "s" 'evil-snipe-s
  "S" 'evil-snipe-S)

(evil-define-key 'operator evil-snipe-local-mode-map
  "z" 'evil-snipe-s
  "Z" 'evil-snipe-S
  "x" 'evil-snipe-x
  "X" 'evil-snipe-X)

(evil-define-key 'motion evil-snipe-override-local-mode-map
  "f" 'evil-snipe-f
  "F" 'evil-snipe-F
  "t" 'evil-snipe-t
  "T" 'evil-snipe-T)

(when evil-snipe-override-evil-repeat-keys
  (evil-define-key 'motion map
    "," 'evil-snipe-repeat
    "," 'evil-snipe-repeat-reverse))
						 )
