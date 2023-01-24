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

(defun comment_end_of_line ()
  (interactive)
  (call-interactively 'comment-dwim)
  (call-interactively 'evil-append))

(evil-define-key 'normal 'global (kbd "gcA") 'comment_end_of_line)
(evil-define-key 'normal 'global (kbd "gcc") 'comment-line)
(evil-define-key 'visual 'global (kbd "gc") 'comment-line)

(use-package avy
	:straight t
	:config)

(evil-define-key 'normal 'global (kbd "s") 'avy-goto-char-2)
(evil-define-key 'motion 'global (kbd "s") 'avy-goto-char-2)
(evil-define-key 'operator 'global (kbd "s") 'avy-goto-char-2)

(evil-define-key 'normal 'global (kbd "f") 'avy-goto-char-in-line)
(evil-define-key 'motion 'global (kbd "f") 'avy-goto-char-in-line)
(evil-define-key 'operator 'global (kbd "f") 'avy-goto-char-in-line) ;

(use-package which-key
	:straight t
	:init
	(which-key-setup-minibuffer)
	(which-key-mode))
