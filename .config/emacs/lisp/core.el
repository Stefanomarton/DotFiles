(provide 'core)

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
	:custom (evil-collection-setup-minibuffer t) ; enable evil mode in minibuffer
	:config
	(evil-collection-init))

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

;; Use escape to remove hightlight in normal mode
(evil-define-key 'normal 'global (kbd "<escape>") 'evil-ex-nohighlight)
(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'visual (kbd "SPC"))
(evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
(evil-define-key 'normal 'global (kbd "<leader>fw") 'find-file-other-window)
(evil-define-key 'normal 'global (kbd "<leader>fg") 'consult-grep)
(evil-define-key 'normal 'global (kbd "<leader>bb") 'consult-buffer)
(evil-define-key 'normal 'global (kbd "<leader>bw") 'consult-buffer-other-window)
(evil-define-key 'normal 'global (kbd "<leader>w") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>q") 'evil-quit)
(evil-define-key 'normal 'global (kbd "<leader>sv") 'split-and-follow-vertically)
(evil-define-key 'normal 'global (kbd "<leader>sh") 'split-and-follow-horizontally)
(evil-define-key 'normal 'global (kbd "<leader>gg") 'google-this)
(evil-define-key 'visual 'global (kbd "<leader>gg") 'google-this-noconfirm)

(use-package evil-goggles
	:straight t
	:after evil
	:config
	(evil-goggles-mode)
	(setq evil-goggle-duration 0.2)
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

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
	:init
	(savehist-mode))

;; Enable vertico
(use-package vertico
	:straight t
	:init
	(vertico-mode)
	(setq vertico-count 20)
	(setq vertico-scroll-margin 3)
	)

(use-package marginalia
	:config
	(marginalia-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
	:straight t
	:init
	(savehist-mode))

;; A few more useful configurations...
(use-package emacs
	:init
	;; Add prompt indicator to `completing-read-multiple'.
	;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
	(defun crm-indicator (args)
		(cons (format "[CRM%s] %s"
									(replace-regexp-in-string
									 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
									 crm-separator)
									(car args))
					(cdr args)))
	(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

	;; Do not allow the cursor in the minibuffer prompt
	(setq minibuffer-prompt-properties
				'(read-only t cursor-intangible t face minibuffer-prompt))
	(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

	;; Emacs 28: Hide commands in M-x which do not work in the current mode.
	;; Vertico commands are hidden in normal buffers.
	;; (setq read-extended-command-predicate
	;;       #'command-completion-default-include-p)

	;; Enable recursive minibuffers
	(setq enable-recursive-minibuffers t))

;; Optionally use the `orderless' completion style.
(use-package orderless
	:straight t
	:init
	;; Configure a custom style dispatcher (see the Consult wiki)
	;; (setq orderless-style-dispatchers '(+orderless-dispatch)
	;;       orderless-component-separator #'orderless-escapable-split-on-space)
	(setq completion-styles '(orderless basic)
				completion-category-defaults nil
				completion-category-overrides '((file (styles partial-completion)))))

;; Vertico postframe
(use-package vertico-posframe
	:straight t
	:init
	:config
	(setq vertico-posframe-min-width 50)
	(setq vertico-posframe-width 123)
	(vertico-posframe-mode 1)
	;; (setq vertico-posframe-parameters
	;;     '((left-fringe . 8)
	;;       (right-fringe . 8)))
	)

(evil-define-key 'normal 'global (kbd ":") 'execute-extended-command)

(use-package doom-modeline
	:straight t
	:init (doom-modeline-mode 1))

(use-package lsp-mode
	:straight t
	:config
	(setq lsp-enable-symbol-highlighting nil)
	(setq lsp-lens-enable nil)
	;; (setq lsp-headerline-breadcrumb-enable nil)
	(setq lsp-keymap-prefix "C-c l")
	:hook (
				 (lua-mode . lsp)
				 (lsp-mode . lsp-enable-which-key-integration)) ;whichkey-integration
	:commands lsp)

(use-package lua-mode
	:straight t)

;; optionally
(use-package lsp-ui
	:straight t
	:commands lsp-ui-mode)

(use-package company
	:straight t
	:init
	(add-hook 'after-init-hook 'global-company-mode))

;; Editorconfig, auto set indenting
(use-package editorconfig
	:config
	(editorconfig-mode 1))

;; Autopair
(electric-pair-mode 1)

;; Highlight nested parentheses (from Jamie's)
(use-package rainbow-delimiters
	:config
	(add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Highlight colorstring with the right color
(use-package rainbow-mode
	:config
	(add-hook 'prog-mode #'rainbow-mode))

(use-package consult)
(use-package embark)
(use-package embark-consult)
(use-package dashboard
	:ensure t
	:config
	(dashboard-setup-startup-hook))

(use-package google-this)
