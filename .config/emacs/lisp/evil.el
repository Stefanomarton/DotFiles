;;; evil.el --- Evil configuration

(use-package evil
  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (setq completion-in-region-function 'consult-completion-in-region)
  (setq evil-want-integration nil)
  (setq evil-echo-state nil)
  (setq evil-jumps-cross-buffers t)
  (setq evil-want-empty-ex-last-command t)
  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (setq evil-want-C-d-scroll nil) ;; avoid scroll down with 'C-d'
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-search-module 'evil-search)
  (setq evil-want-fine-undo t)

  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))
  ;; Use escape to remove hightlight in normal mode
  (evil-define-key 'normal 'global (kbd "<escape>") 'evil-ex-nohighlight)
  (evil-define-key 'normal 'global (kbd "C-u") 'evil-scroll-up)
  (evil-define-key 'normal 'global (kbd "H") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "L") 'evil-window-right)
  (evil-define-key 'normal 'global (kbd "m") 'point-to-register)
  (evil-define-key 'normal 'global (kbd "'") 'jump-to-register)
  (evil-define-key 'normal 'global (kbd "L") 'evil-window-right)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))
  (evil-define-key 'insert 'global (kbd "C-y") 'evil-paste-after)

  ;; Undoing each character entered in insert mode one by one.
  (advice-add 'undo-auto--last-boundary-amalgamating-number
              :override #'ignore)

  (evil-define-key 'normal 'global (kbd "C-d") 'evil-scroll-down)

  ;; Use escape to remove hightlight in normal mode
  (evil-define-key 'normal 'global (kbd "<escape>") 'evil-ex-nohighlight)

  (defun smart-for-files ()
    (interactive)
    (if (projectile-project-p)
        (consult-projectile-find-file)
      (call-interactively #'find-file)))

  (defun smart-for-buffer ()
    (interactive)
    (if (projectile-project-p)
        (consult-projectile-switch-to-buffer)
      (call-interactively #'consult-buffer)))

  (defun smart-for-terminal-otherw ()
    (interactive)
    (if (projectile-project-p)
        (projectile-run-vterm-other-window)
      (call-interactively #'vterm-other-window)))

  (defun smart-for-terminal ()
    (interactive)
    (if (projectile-project-p)
        (projectile-run-vterm)
      (call-interactively #'vterm)))

  (defun split-and-follow-horizontally ()
    (interactive)
    (split-window-below)
    (balance-windows)
    (other-window 1))

  (defun split-and-follow-vertically ()
    (interactive)
    (split-window-right)
    (balance-windows)
    (other-window 1))

  (defun my-kill-this-buffer ()
    (interactive)
    (catch 'quit
      (save-window-excursion
        (let (done)
	      (when (and buffer-file-name (buffer-modified-p))
	        (while (not done)
	          (let ((response (read-char-choice
			                   (format "Save file %s? (y, n, d, q) " (buffer-file-name))
			                   '(?y ?n ?d ?q))))
	            (setq done (cond
			                ((eq response ?q) (throw 'quit nil))
			                ((eq response ?y) (save-buffer) t)
			                ((eq response ?n) (set-buffer-modified-p nil) t)
			                ((eq response ?d) (diff-buffer-with-file) nil))))))
	      (kill-buffer (current-buffer))))))

  (evil-define-key 'insert 'global (kbd "C-y") 'evil-paste-after)
  (define-key evil-normal-state-map (kbd "Y") (kbd "y$"))
  (evil-define-key 'normal 'global (kbd "C-u") 'evil-scroll-up)
  (evil-define-key 'normal 'global
    (kbd ";") 'evil-ex
    ;; (kbd ":") 'execute-extended-command
    (kbd ":") 'execute-extended-command
    (kbd "<leader>ff") 'smart-for-files
    (kbd "<leader>fw") 'find-file-other-window
    (kbd "<leader>fr") 'consult-recent-file
    (kbd "<leader>fg") 'consult-ripgrep
    (kbd "<leader>dj") 'dirvish
    (kbd "<leader>dD") 'dired
    (kbd "<leader>dd") 'consult-dir
    (kbd "<leader>bb") 'smart-for-buffer
    (kbd "<leader>w") 'save-buffer
    (kbd "<leader> q b") 'kill-buffer
    (kbd "Q") 'my-kill-this-buffer
    (kbd "C-s v") 'split-and-follow-vertically
    (kbd "C-s h") 'split-and-follow-horizontally
    (kbd "<leader>gt") 'google-this
    (kbd "<leader>gh") 'dashboard-open
    (kbd "<leader>pp") 'consult-projectile
    (kbd "<leader>ps") 'consult-projectile-switch-project
    (kbd "<leader>cc") 'calc
    (kbd "<leader>qc") 'quick-calc
    (kbd "<leader>t") 'smart-for-terminal-otherw
    (kbd "<leader>T") 'smart-for-terminal
    (kbd "<leader>gg") 'magit-status-with-removed-dotfiles-args
    (kbd "<leader>gd") 'dotfiles-magit-status
    )

  (evil-define-key 'normal emacs-lisp-mode-map
    (kbd "<leader>ee") 'eval-buffer
    (kbd "<leader>es") 'eval-expression
    (kbd "<leader>er") 'eval-region
    (kbd "<leader>ef") 'eval-defun
    (kbd "<leader>eb") 'byte-compile-file
    (kbd "<leader>eB") 'byte-recompile-directory
    )

  (evil-define-key '(normal visual) 'global
    (kbd "<leader> nn") 'narrow-to-region
    (kbd "<leader> nw") 'widen
    (kbd "<leader> np") 'narrow-to-page
    (kbd "<leader> nf") 'narrow-to-defun
    (kbd "<leader> ng") 'goto-line-relative
    )

  (evil-define-key 'insert 'global (kbd "C-<backspace>") 'evil-delete-backward-word)
  (evil-define-key 'visual 'global (kbd "<leader>gg") 'google-this-noconfirm)
  (evil-define-key 'normal 'prog-mode-map (kbd "<leader>m") 'rainbow-mode)

  (evil-mode 1))

;; Undo for evil
(use-package undo-fu
  :after evil)

;; Persisten undo
(use-package undo-fu-session
  :after undo-fu
  :config
  (undo-fu-session-global-mode))

;; Evil integrations
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :after evil
  :config
  (evil-define-key '(normal visual) 'global (kbd ",") 'evil-surround-edit)
  (add-hook 'prog-mode-hook (lambda ()
			                  (push '(?\( . ("\(" . "\)")) evil-surround-pairs-alist)))
  (add-hook 'org-mode-hook (lambda ()
			                 (push '(?\( . ("\(" . "\)")) evil-surround-pairs-alist)))
  (add-hook 'LaTeX-mode-hook (lambda ()
			                   (push '(?\( . ("\(" . "\)")) evil-surround-pairs-alist)))
  (add-hook 'markdown-mode-hook (lambda ()
				                  (push '(?\( . ("\(" . "\)")) evil-surround-pairs-alist)))
  )

(use-package evil-commentary
  ;; Better Comment Action
  :after evil
  :config
  (defun comment_end_of_line ()
    (interactive)
    (call-interactively 'comment-dwim)
    (call-interactively 'evil-append))
  (evil-define-key 'visual 'global (kbd "gc") 'evil-commentary)
  (evil-define-key 'normal 'global (kbd "gcA") 'comment_end_of_line)
  (evil-define-key 'visual 'global (kbd "gb") 'comment-box)
  (evil-define-key 'normal 'global (kbd "gcc") 'evil-commentary-line))

(use-package evil-tex
  :defer t
  :hook
  (LaTeX-mode . evil-tex-mode)
  ;; :config
  ;; (setq evil-tex-toggle-override-m t)
  )

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  (setq evil-goggle-duration 0.5)
  (evil-goggles-use-diff-faces))

(provide 'evil)

;;; evil.el ends here