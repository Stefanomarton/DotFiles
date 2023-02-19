(provide 'core)

(use-package evil
	:straight t
	:init
	(setq evil-want-integration t)
	(setq evil-want-keybinding nil)
	(setq evil-undo-system 'undo-fu)
	(setq evil-search-module 'evil-search)
	:config
	(modify-syntax-entry ?_ "w")
	(modify-syntax-entry ?- "w")
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
(evil-define-key 'normal 'global (kbd "<leader>tt") 'consult-theme)
(evil-define-key 'normal 'global (kbd "<leader>w") 'save-buffer)
(evil-define-key 'normal 'global (kbd "<leader>q") 'evil-quit)
(evil-define-key 'normal 'global (kbd "<leader>sv") 'split-and-follow-vertically)
(evil-define-key 'normal 'global (kbd "<leader>sh") 'split-and-follow-horizontally)
(evil-define-key 'normal 'global (kbd "<leader>gg") 'google-this)
(evil-define-key 'visual 'global (kbd "<leader>gg") 'google-this-noconfirm)
(evil-define-key 'normal 'global (kbd "<leader>l") 'evil-window-right)
(evil-define-key 'normal 'global (kbd "<leader>h") 'evil-window-left)
(evil-define-key 'normal 'global (kbd "S") 'evil-surround-edit)
(evil-define-key 'normal 'global (kbd ",r") 'evil-surround-delete)
(evil-define-key 'normal 'global (kbd ",c") 'evil-surround-change)

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
	:custom
	(add-to-list 'company-backends 'company-math-symbols-latex)
	(add-to-list 'company-backends 'company-auctex)
	:init
	(add-hook 'after-init-hook 'global-company-mode))

;; (use-package corfu
;;	:custom
;;	(corfu-cycle t)
;;	(corfu-auto t)
;;	(corfu-auto-prefix 2)
;;	(corfu-auto-delay 0.0)
;;	:init
;;	(global-corfu-mode))


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
(use-package org-bullets
	:config
	;; use org-bullets-mode for utf8 symbols as org bullets
	(require 'org-bullets)
	;; make available "org-bullet-face" such that I can control the font size individually
	(setq org-bullets-face-name (quote org-bullet-face))
	(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
	(setq org-bullets-bullet-list '("✡" "⎈" "✽" "✲" "✱" "✻" "✼" "✽" "✾" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇"))
	(setq org-hide-emphasis-markers t)
	(custom-set-faces
	 '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
	 '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
	 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
	 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
	 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
	(setq org-agenda-files '("~/org"))
	:init
	(add-hook 'org-mode-hook 'visual-line-mode)
	(add-hook 'org-mode-hook 'org-indent-mode)
	(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package tab-jump-out
	:custom
	(tab-jump-out-mode 1))

(straight-use-package '(targets :type git :host github
																:repo "dvzubarev/targets.el"
																:branch "fix-remote"))

(use-package targets
	:config
	(setq targets-text-objects nil)
	(targets-setup nil)
	(targets-define-composite-to any-block
		(("(" ")" pair)
		 ("[" "]" pair)
		 ("{" "}" pair)
		 ;; ("<" ">" pair)
		 )
		:bind t
		:next-key "N"
		:last-key "L"
		:around-key nil
		:inside-key nil
		:keys "b")
	(targets-define-composite-to any-quote
		(("\"" "\"" quote)
		 ("'" "'" quote))
		:bind t
		:next-key "N"
		:last-key "L"
		:around-key nil
		:inside-key nil
		:keys "q")
	(targets-define-to word 'evil-word nil object :bind t :keys "w")
	(targets-define-to double-quote
										 "\"" nil quote
										 :bind t
										 :next-key "N"
										 :last-key "L"
										 :around-key nil
										 :inside-key nil
										 :keys "q"
										 :hooks (emacs-lisp-mode-hook)))

(use-package nyan-mode)

(use-package org-download
	:hook
	(add-hook 'dired-mode-hook 'org-download-enable))

(use-package markdown-mode
	:mode ("README\\.md\\'" . gfm-mode)
	:init
	(setq markdown-enable-math t))
;; :init (setq markdown-command "multimarkdown"))


;;;;;;;;;;;
;; Latex ;;
;;;;;;;;;;;

(use-package laas
	:straight (laas :type git :host github :repo "Stefanomarton/LaTeX-auto-activating-snippets")
	:hook (LaTeX-mode . laas-mode)
	:hook (markdown-mode . laas-mode)
	:config ; do whatever here
	(aas-set-snippets 'laas-mode
										;; set condition!
										:cond #'texmathp ; expand only while in math
										"supp" "\\supp"
										"On" "O(n)"
										"O1" "O(1)"
										"Olog" "O(\\log n)"
										"Olon" "O(n \\log n)"
										;; bind to functions!
										"sum" (lambda () (interactive)
														(yas-expand-snippet "\\sum_{$1}^{$2} $0"))
										"Span" (lambda () (interactive)
														 (yas-expand-snippet "\\Span($1)$0"))
										;; add accent snippets
										:cond #'laas-object-on-left-condition
										"qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

(straight-use-package 'auctex
											:hook
											(add-hook 'LaTeX-mode-hook 'prettify-symbols-mode))
(add-hook 'LaTeX-mode 'reftex-mode)



;; Yasnippet settings

(setq yas-snippet-dirs '("~/.config/emacs/snippets"))
(use-package yasnippet
	:ensure t
	:hook ((LaTeX-mode . yas-minor-mode)
				 (post-self-insert . my/yas-try-expanding-auto-snippets))
	:config
	(yas-global-mode)
	(use-package warnings
		:config
		(cl-pushnew '(yasnippet backquote-change)
								warning-suppress-types
								:test 'equal))

	(setq yas-triggers-in-field t)

	;; Function that tries to autoexpand YaSnippets
	;; The double quoting is NOT a typo!
	(defun my/yas-try-expanding-auto-snippets ()
		(when (and (boundp 'yas-minor-mode) yas-minor-mode)
			(let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
				(yas-expand)))))

(use-package format-all
	:init
	(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
	(add-hook 'prog-mode-hook 'format-all-mode)
	(add-hook 'LaTeX-mode-hook 'format-all-mode)
	)
(use-package consult-reftex
	:straight (consult-reftex :type git :host github :repo "karthink/consult-reftex")
	:after (reftex consult embark)
	:bind (:map reftex-mode-map
							("C-c )"   . consult-reftex-insert-reference)
							("C-c M-." . consult-reftex-goto-label))
	:config (setq consult-reftex-preview-function
								#'consult-reftex-make-window-preview))

(use-package company-math)
(use-package company-auctex
	:init
	(company-auctex-init))
(use-package company-reftex)
(use-package reftex)

;;;;;;;;;;;;;;;
;; AvyConfig ;;
;;;;;;;;;;;;;;;


(use-package avy
	:straight t
	:config
	(setq avy-timeout-seconds 0.2)
	(setq avy-keys (nconc (number-sequence ?a ?z)))
	)


;; (evil-define-key 'operator 'global (kbd "l") 'avy-goto-line)

(evil-define-key 'normal 'global (kbd "s") 'evil-avy-goto-char-timer)
(evil-define-key 'motion 'global (kbd "s") 'evil-avy-goto-char-timer)
(evil-define-key 'operator 'global (kbd "s") 'evil-avy-goto-char-timer)
(evil-define-key 'normal 'global (kbd "f") 'evil-avy-goto-char-in-line)
(evil-define-key 'motion 'global (kbd "f") 'evil-avy-goto-char-in-line)
(evil-define-key 'operator 'global (kbd "f") 'evil-avy-goto-char-in-line)
(evil-define-key 'normal 'global (kbd "C-SPC") 'er/expand-region)
(evil-define-key 'visual 'global (kbd "SPC SPC") 'er/expand-region)
;; (evil-define-key 'motion 'global (kbd "L") 'avy-copy-line)
(evil-define-key 'operator 'global (kbd "R") 'avy-copy-region)

(setq avy-timeout-seconds 0.25)

(use-package expand-region)

(use-package org-fragtog)

;; Daemon mode configs
(pcase system-type
	('gnu/linux "It's Linux!")
	('windows-nt "It's Windows!")
	('darwin "It's macOS!"))

(if (daemonp)
		(message "Loading in the daemon!")
	(message "Loading in regular Emacs!"))

(defun efs/set-font-faces ()
	(message "Setting faces!")
	(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 150)

	;; Set the fixed pitch face
	(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 150)

	;; Set the variable pitch face
	(set-face-attribute 'variable-pitch nil :font "JetBrainsMono Nerd Font" :height 150 :weight 'regular))

(if (daemonp)
		(add-hook 'after-make-frame-functions
							(lambda (frame)
								;; (setq doom-modeline-icon t)
								(with-selected-frame frame
									(efs/set-font-faces))))
	(efs/set-font-faces)
	(setq highlight-indent-guides-method 'character)
	(setq doom-modeline-icon t)
	)

;; Terminal specific configuration

(if (display-graphic-p)
		;; GUI mode
		(progn
			(nyan-mode 1))
	;; Terminal mode
	())

;; (if (daemonp)
;;		(add-hook 'after-make-frame-functions
;;							(defun my/theme-init-daemon (frame)
;;								(with-selected-frame frame
;;									(load-theme 'atom-one-dark))
;;								;; Run this hook only once.
;;								(remove-hook 'after-make-frame-functions
;;														 #'my/theme-init-daemon)
;;								(fmakunbound 'my/theme-init-daemon)))
;;	(load-theme 'doom-nord t))
