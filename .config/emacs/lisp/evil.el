;;; evil.el --- Evil configuration

(use-package evil
  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (setq completion-in-region-function 'consult-completion-in-region)
  (setq evil-want-integration nil)
  (setq evil-operator-state-cursor nil)
  (setq evil-echo-state nil)
  (setq evil-jumps-cross-buffers t)
  (setq evil-want-empty-ex-last-command t)
  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (setq evil-want-C-d-scroll nil) ;; avoid scroll down with 'C-d'
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-search-module 'evil-search)
  (setq evil-want-fine-undo t)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-esc-delay 0.0001)
  (setq evil-move-beyond-eol t)
  (setq evil-want-Y-yank-to-eol t)
  ;; (setq evil-cross-lines t)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))
  ;; Use escape to remove hightlight in normal mode
  (evil-define-key 'normal 'global (kbd "<escape>") 'evil-ex-nohighlight)
  (evil-define-key 'normal 'global (kbd "C-u") 'evil-scroll-up)

  (evil-define-key 'normal 'global (kbd "J") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "?") 'evil-window-right)

  (evil-define-key 'normal 'global (kbd "h") 'evil-search-forward)
  (evil-define-key 'normal 'global (kbd "H") 'evil-search-backward)
  (evil-define-key '(normal visual replace operator motion emacs) 'global
    (kbd "j") 'evil-backward-char
    (kbd "k") 'evil-next-visual-line
    (kbd "l") 'evil-previous-visual-line
    (kbd "/") 'evil-forward-char
    )
  (evil-define-key 'normal 'global (kbd "C-m") 'point-to-register)
  (evil-define-key 'normal 'global (kbd "gm") 'jump-to-register)
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

  ;; Recompile and reload yasnippet files.
  (defun yas-reload ()
    (interactive)
    (yas-recompile-all)
    (yas-reload-all)
    )

  (evil-define-key 'insert 'global (kbd "C-y") 'evil-paste-after)
  (evil-define-key 'normal 'global (kbd "C-u") 'evil-scroll-up)
  (evil-define-key 'normal 'global
    (kbd ";") 'evil-ex
    ;; (kbd ":") 'execute-extended-command
    (kbd ":") 'execute-extended-command
    (kbd "<leader>ff") 'find-file
    (kbd "<leader>fw") 'find-file-other-window
    (kbd "<leader>fr") 'consult-recent-file
    (kbd "<leader>fg") 'consult-grep
    (kbd "<leader>r") 'consult-register
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
    (kbd "<leader>co") 'consult-outline
    (kbd "<leader>cm") 'consult-global-mark
    (kbd "<leader>ci") 'consult-imenu

    (kbd "<leader>aw") 'avy-goto-word-or-subword-1
    (kbd "<leader>al") 'avy-goto-line
    (kbd "<leader>arm") 'avy-move-region
    (kbd "<leader>arc") 'avy-copy-region
    (kbd "<leader>ark") 'avy-kill-region

    (kbd "<leader>qc") 'quick-calc
    (kbd "<leader>t") 'smart-for-terminal-otherw
    (kbd "<leader>T") 'smart-for-terminal
    (kbd "<leader>gg") 'magit-status-with-removed-dotfiles-args
    (kbd "<leader>gd") 'dotfiles-magit-status
    (kbd "<leader>h") 'consult-todo
    (kbd "<leader>H") 'consult-todo-all
    (kbd "<leader>y") 'yas-reload
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
    (kbd "<leader> df") 'evil-goto-definition
    (kbd "gu") 'evil-next-close-paren
    (kbd "gd") 'evil-previous-open-paren
    (kbd "gD") 'evil-previous-open-brace
    (kbd "gU") 'evil-next-close-brace
    )

  (evil-define-key 'insert 'global (kbd "C-<backspace>") 'evil-delete-backward-word)
  (evil-define-key 'visual 'global (kbd "<leader>gg") 'google-this-noconfirm)
  (evil-define-key 'normal 'prog-mode-map (kbd "<leader>m") 'rainbow-mode)

  (evil-mode 1))

;; ;; Undo for evil
(use-package undo-fu
  :after evil)

;; ;; Persisten undo
(use-package undo-fu-session
  :after undo-fu
  :config
  (undo-fu-session-global-mode))

;; Evil integrations
(use-package evil-collection
  :defer .5
  :after dashboard
  :config
  (evil-collection-init))

(use-package evil-terminal-cursor-changer
  :after evil
  :config
  (unless (display-graphic-p)
    (evil-terminal-cursor-changer-activate) ; or (etcc-on)
    ))

(use-package evil-surround
  :defer t
  :commands (evil-surround-edit evil-surround-change evil-surround-delete)
  :init
  (evil-surround-mode)
  :config
  (evil-define-key '(normal visual) 'global (kbd ", ,") 'evil-surround-edit)
  (evil-define-key '(normal visual) 'global (kbd ", c") 'evil-surround-change)
  (evil-define-key '(normal visual) 'global (kbd ", d") 'evil-surround-delete)
  (add-hook 'prog-mode-hook (lambda ()
 			                  (push '(?\( . ("\(" . "\)")) evil-surround-pairs-alist)))
  (add-hook 'org-mode-hook (lambda ()
 			                 (push '(?\( . ("\(" . "\)")) evil-surround-pairs-alist)))
  (add-hook 'LaTeX-mode-hook (lambda ()
 			                   (push '(?\( . ("\(" . "\)")) evil-surround-pairs-alist)))
  (add-hook 'markdown-mode-hook (lambda ()
 				                  (push '(?\( . ("\(" . "\)")) evil-surround-pairs-alist)))

  (add-hook 'markdown-mode-hook (lambda ()
                                  (push '(?* . ("**" . "**")) evil-surround-pairs-alist))))

(use-package evil-commentary
  ;; Better Comment Action
  :defer t
  :commands (evil-commentary evil-commentary-line)
  :after dashboard
  :config
  ;; Comment at the end of the line
  (defun comment-end-of-line ()
    (interactive)
    (indent-for-comment)
    (evil-insert 1)
    )
  (evil-define-key 'normal 'global (kbd "gcA") 'comment-end-of-line)

  ;; Comment box
  (evil-define-key 'visual 'global (kbd "gb") 'comment-box)

  ;; Comment regione
  (evil-define-key 'visual 'global (kbd "gc") 'evil-commentary)

  ;; Comment current line
  (evil-define-key 'normal 'global (kbd "gcc") 'evil-commentary-line)

  ;; Open a command above
  (defun comment-line-above ()
    (interactive)
    (evil-open-above 1)
    (insert (concat comment-start))
    )
  (evil-define-key 'normal 'global (kbd "gco") 'comment-line-above)

  ;; Open a command below
  (defun comment-line-above ()
    (interactive)
    (evil-open-below 1)
    (insert (concat comment-start))
    )
  (evil-define-key 'normal 'global (kbd "gcO") 'comment-line-above)
  )

(use-package evil-tex
  :defer t
  :hook
  (LaTeX-mode . evil-tex-mode)
  (org-mode . evil-tex-mode)
  ;; :config
  ;; (setq evil-tex-toggle-override-m t)
  )

(use-package more-evil-avy
  :after avy
  :straight (:host github :repo "Stefanomarton/more-evil-avy"))

(use-package general
  :after consult
  :config
  (defun my-evil-change-whole-line ()
    (interactive)
    (beginning-of-line)
    (evil-change-line (point) (line-end-position)))

  (defun my-evil-change-visual-selection ()
    "Replace the region with an empty line and enter insert mode."
    (interactive)
    (let ((start (region-beginning))
          (end (region-end)))
      (delete-region start end)
      (goto-char start)
      (open-line 1)
      (evil-insert 1)))
  (general-evil-setup t)

  (general-nmap "c" (general-key-dispatch 'evil-change
                      "c" 'my-evil-change-whole-line))
  (general-vmap "c" 'my-evil-change-visual-selection)
  )

(use-package evil-owl
  :after evil
  :diminish ""
  :config
  (setq evil-owl-idle-delay 0.8)
  (setq evil-owl-display-method 'posframe
        evil-owl-extra-posframe-args '(:width 50 :height 20)
        evil-owl-max-string-length 50)
  ;; (add-to-list 'display-buffer-alist
  ;;              '("*evil-owl*"
  ;;                (display-buffer-in-side-window)
  ;;                (side . bottom)
  ;;                (window-height . 20)))
  (evil-owl-mode)
  )


;; go to last edit with g;
(use-package goto-chg
  :after evil
  :config
  (evil-define-key 'normal 'global
    (kbd "gk") 'evil-goto-last-change
    (kbd "gl") 'evil-goto-last-change-reverse))

;; better object with h
(use-package evil-textobj-syntax
  :after evil
  )

;; matching tag with %
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1)
  )

(provide 'evil)

;;; evil.el ends here
