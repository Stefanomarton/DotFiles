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
  :straight t)

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
	:straight t
	:config 
(evil-commentary-mode))

(use-package avy
	:straight t
	:config)

(evil-define-key 'normal 'global (kbd "s") 'avy-goto-char-2)
(evil-define-key 'motion 'global (kbd "s") 'avy-goto-char-2)
(evil-define-key 'operator 'global (kbd "s") 'avy-goto-char-2)

(evil-define-key 'normal 'global (kbd "f") 'avy-goto-char-in-line)
(evil-define-key 'motion 'global (kbd "f") 'avy-goto-char-in-line)
(evil-define-key 'operator 'global (kbd "f") 'avy-goto-char-in-line)

