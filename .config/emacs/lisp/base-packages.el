;;; base-packages.el -*- lexical-binding: t; -*-
(use-package bind-key)

;; Recent file list
(use-package recentf
  :defer .5
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
  :defer 1
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
  :defer .1
  :after vertico
  :init
  (marginalia-mode))

(use-package consult
  :init
  (setq consult-preview-allowed-hooks '(global-font-lock-mode-check-buffers save-place-find-file-hook display-line-numbers-mode))
  :defer 1)

(use-package consult-projectile
  :after (consult projectile))

(use-package consult-dir
  :after consult)

(use-package consult-org-roam
  :after consult)

(use-package consult-todo
  :straight (:host github :repo "liuyinz/consult-todo")
  :after consult
  :config
  (defconst consult-todo--narrow
    '((?t . "TODO")
      (?f . "FIXME")
      (?b . "BUG")
      (?h . "ASK"))
    "Default mapping of narrow and keywords.")
  )

(use-package hl-todo
  :after dashboard
  :config
  (global-hl-todo-mode)
  :custom
  (hl-todo-keyword-faces
   '(("TODO"   . "#FF0000")
     ("FIXME"  . "#FF0000")
     ("ASK"  . "#A020F0")
     ("GOTCHA" . "#FF4500")
     ("STUB"   . "#1E90FF"))))

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
  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'embark-minimal-act)
  (evil-define-key 'normal 'global (kbd "C-.") 'embark-dwim)
  (evil-define-key 'insert 'global (kbd "C-.") 'embark-minimal-act)
  (evil-define-key 'visual 'global (kbd "<leader>SPC") 'embark-dwim)

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
  (put 'dired-find-alternate-file 'disabled nil)

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
  (setq dirvish-default-layout '(0.8 0.2 0.5))
  :init
  (dirvish-override-dired-mode)
  )

;; Nice auto formatting
(use-package format-all
  :after dashboard
  :config
  (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
  (add-hook 'prog-mode-hook 'format-all-mode)
  (add-hook 'LaTeX-mode-hook 'format-all-mode)
  :custom
  (format-all-show-errors 'error)
  )

(use-package posframe
  :after dashboard)

(use-package hydra
  :after dashboard)

(use-package move-dup
  :config
  (global-set-key (kbd "<leader>l") 'move-dup-duplicate-up)
  (global-set-key (kbd "<leader>k") 'move-dup-duplicate-down)
  )
