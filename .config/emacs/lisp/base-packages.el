;;; base-packages.el -*- lexical-binding: t; -*-
(use-package bind-key)

;; Go evil
(use-package evil
  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))
  (setq evil-want-integration nil)
  (setq evil-echo-state nil)
  (setq evil-want-empty-ex-last-command t)
  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (setq evil-want-C-d-scroll nil) ;; avoid scroll down with 'C-d'
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-search-module 'evil-search)
  (setq evil-want-fine-undo t)

  ;; Use escape to remove hightlight in normal mode
  (evil-define-key 'normal 'global (kbd "<escape>") 'evil-ex-nohighlight)
  (evil-define-key 'insert 'global (kbd "C-y") 'evil-paste-after)
  (evil-define-key 'normal 'global (kbd "C-u") 'evil-scroll-up)
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))

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
    (kbd "<leader>ef") 'eval-defun)

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

;; Better help
;;(use-package helpful
;;  :commands (helpful-callable helpful-variable helpful-command helpful-key)
;;  :bind
;;  ([remap describe-command] . helpful-command)
;;  ([remap describe-key] . helpful-key))

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
  :requires evil
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
  :after evil
  :hook
  (LaTeX-mode . evil-tex-mode)
  :config
  (setq evil-tex-toggle-override-m t)
  )

(use-package evil-goggles
  :after evil
  :config
  (evil-goggles-mode)
  (setq evil-goggle-duration 0.5)
  (evil-goggles-use-diff-faces))

;; Recent file list
(use-package recentf
  :after evil
  :config
  (add-hook 'emacs-startup-hook 'recentf-mode)
  (add-hook 'after-init-hook
            (lambda ()
	          (setq inhibit-message t)
	          (run-with-idle-timer 0 nil (lambda () (setq inhibit-message nil)))))
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-saved-items 25))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :custom
  (which-key-allow-evil-operators t)
  (which-key-show-remaining-keys t)
  (which-key-sort-order 'which-key-prefix-then-key-order)
  (which-key-idle-delay 1)
  :config
  (which-key-mode)
  (which-key-setup-minibuffer))

;; Enable vertico for the best vertical completion experience
(use-package vertico
  :defer 1
  :bind
  (:map vertico-map
	    ("C-e" . embark-minimal-act)
	    ("C-j" . vertico-next)
	    ("C-k" . vertico-previous)
	    ("<escape>" . keyboard-escape-quit))
  :init
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	    '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)
  :config
  ;; Indicated in the documentation
  (setq fast-but-imprecise-scrolling t
	    jit-lock-defer-time 0)

  ;; Ignore case when completing
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  ;; Different scroll margin
  (setq vertico-scroll-margin 2)

  ;; Show more candidates
  (setq vertico-count 10)
  (setq vertico-resize nil)
  (setq vertico-cycle t)
  :init
  (vertico-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :config
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
	    completion-category-defaults nil
	    completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :defer t
  :after vertico
  :config
  (marginalia-mode))

(use-package consult
  :defer 1)

(use-package consult-projectile
  :after (consult projectile))

(use-package consult-dir
  :after consult)

(use-package flycheck
  :defer t
  )

(use-package consult-flycheck
  :after (consult flycheck))

(use-package consult-flyspell
  :after consult)

(use-package embark
  :after dashboard
  :config

  ;; Base keybindings
  (evil-define-key 'normal 'global (kbd "m") 'embark-minimal-act)
  (evil-define-key 'normal 'global (kbd "M") 'embark-dwim)
  (evil-define-key 'insert 'global (kbd "C-e") 'embark-minimal-act)
  (evil-define-key 'visual 'global (kbd "M") 'embark-dwim)

  ;; Which-key style indicator
  (defun embark-minimal-act (&optional arg)
    (interactive "P")
    (let ((embark-indicators
	       '(embark-which-key-indicator
	         embark-highlight-indicator
	         embark-isearch-highlight-indicator)))
      (embark-act arg)))

  (defun embark-minimal-act-noexit ()
    (interactive)
    (embark-minimal-act 4))
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	           '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		         nil
		         (window-parameters (mode-line-format . none))))
  (add-to-list 'embark-indicators #'embark-which-key-indicator)
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
	      (which-key--hide-popup-ignore-command)
	    (which-key--show-keymap
	     (if (eq (caar targets) 'embark-become)
	         "Become"
	       (format "Act on %s '%s'%s"
		           (plist-get (car targets) :type)
		           (embark--truncate-target (plist-get (car targets) :target))
		           (if (cdr targets) "…" "")))
	     (if prefix
	         (pcase (lookup-key keymap prefix 'accept-default)
	           ((and (pred keymapp) km) km)
	           (_ (key-binding prefix 'accept-default)))
	       keymap)
	     nil nil t))))
  (setq embark-cycle-key "SPC")
  (setq embark-quit-after-action t)
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight (:host github :repo "oantolin/embark"
		           :files ("embark-consult.el"))
  :after (embark consult)
  :bind (:map embark-become-file+buffer-map
	          ("m" . consult-bookmark)
	          ("b" . consult-buffer)
	          ("j" . consult-find)))

(use-package dired
  :commands dired
  :straight nil
  :ensure nil
  :config

  (setq dired-kill-when-opening-new-dired-buffer t)

  (evil-define-key 'normal dired-mode-map
    (kbd "f") 'dired-narrow-fuzzy
    (kbd "T") 'dired-create-empty-file
    (kbd "<RET>") 'dired-find-alternate-file
    (kbd "<escape>") 'keyboard-escape-quit
    (kbd "h") 'dired-up-directory))

(use-package dired-narrow
  :after dired
  :config
  (defun dired-narrow-ex-ac ()
    ;; Revert buffer and enter the directory after narrowing
    (revert-buffer)
    (dired-find-alternate-file))
  (setq dired-narrow-exit-when-1-left t)
  (setq dired-narrow-exit-action 'dired-narrow-ex-ac)
  )

;; Better dired
(use-package dirvish
  :commands (dired-jump dirvish-dwim)
  :config
  (evil-define-key 'normal dirvish-mode-map (kbd "q") 'dirvish-quit)
  (evil-define-key 'normal dirvish-mode-map (kbd "<escape>") 'dirvish-layout-toggle)
  :init
  (dirvish-override-dired-mode)
  )

;; Nice auto formatting
(use-package format-all
  :init
  (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
  (add-hook 'prog-mode-hook 'format-all-mode)
  (add-hook 'LaTeX-mode-hook 'format-all-mode)
  )
