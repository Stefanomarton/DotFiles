(provide 'core)

(use-package restart-emacs)
(use-package evil
	:straight t
	:init
	(setq completion-in-region-function 'consult-completion-in-region)
	(setq evil-want-integration t)
	(setq evil-echo-state nil)
	(setq evil-want-keybinding nil)
	(setq evil-want-empty-ex-last-command nil)
	(setq evil-undo-system 'undo-fu)
	(setq evil-search-module 'evil-search)
	:config
	;; (modify-syntax-entry ?_ "w")
	;; (modify-syntax-entry ?- "w")
	(evil-mode 1))

(use-package evil-collection
	:straight t
	:after evil
	;; :custom (evil-collection-setup-minibuffer t) ; enable evil mode in minibuffer
	:config
	(evil-collection-init))

;; breaks the evil undo sequence when the buffer is changed over a line boundary
(use-package evil-nl-break-undo
	:hook ((text-mode prog-mode) . evil-nl-break-undo-mode))

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
(evil-define-key 'insert 'global (kbd "C-y") 'evil-paste-after)
;; (keymap-set vertico-map "C-i" #'vertico-quick-insert)
;; (keymap-set vertico-map "C-j" #'vertico-quick-jump)
(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'visual (kbd "SPC"))

;;Settings normal global keybindings
(evil-define-key 'normal 'global
	(kbd ";") 'evil-ex
	(kbd ":") 'execute-extended-command
	(kbd "<leader>ff") 'find-file
	(kbd "<leader>fw") 'find-file-other-window
	(kbd "<leader>fr") 'consult-recent-file
	(kbd "<leader>fg") 'consult-grep
	(kbd "<leader>dj") 'dired-jump
	(kbd "<leader>dd") 'dired
	(kbd "<leader>bb") 'consult-buffer
	(kbd "<leader>bw") 'consult-buffer-other-window
	(kbd "<leader>w") 'save-buffer
	(kbd "<leader>qb") 'kill-buffer
	(kbd "Q") 'evil-quit
	(kbd "<leader>sv") 'split-and-follow-vertically
	(kbd "<leader>sh") 'split-and-follow-horizontally
	(kbd "<leader>gg") 'google-this
	(kbd "<leader>l") 'evil-window-right
	(kbd "<leader>h") 'evil-window-left
	(kbd "<leader>ee") 'eval-buffer
	(kbd "<leader>es") 'eval-expression
	(kbd "<leader>er") 'eval-region
	(kbd "<leader>er") 'eval-defun
	(kbd "S") 'evil-surround-edit
	(kbd ",r") 'evil-surround-delete
	(kbd ",c") 'evil-surround-change)

(evil-define-key 'insert 'global (kbd "C-<backspace>") 'evil-delete-backward-word)
(evil-define-key 'visual 'global (kbd "<leader>gg") 'google-this-noconfirm)
(evil-define-key 'normal 'prog-mode-map (kbd "<leader>m") 'rainbow-mode)

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
	:config
	(setq vertico-multiform-commands
				'((consult-line
					 posframe
					 (vertico-posframe-poshandler . posframe-poshandler-frame-top-center)
					 (vertico-posframe-border-width . 10)
					 ;; NOTE: This is useful when emacs is used in both in X and
					 ;; terminal, for posframe do not work well in terminal, so
					 ;; vertico-buffer-mode will be used as fallback at the
					 ;; moment.
					 (vertico-posframe-fallback-mode . vertico-buffer-mode))
					(t posframe)))
	(vertico-multiform-mode 1)
	;; Using vertico-multiform-mode is possibile to avoid use of posframe in certain buffer
	;; (setq vertico-multiform-commands
	;;			'((execute-extended-command (:not posframe))
	;;				(t posframe)))
	;; :init
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

;;Vertico postframe
(use-package vertico-posframe
	:straight t
	:init
	:config
	(setq vertico-posframe-min-width 50)
	(setq vertico-posframe-width 123)
	(vertico-posframe-mode 1)
	(setq vertico-posframe-parameters
				'((left-fringe . 10)
					(right-fringe . 10))))

(use-package doom-modeline
	:straight t
	:init (doom-modeline-mode 1))

(use-package lsp-mode
	:straight t
	:config
	(setq lsp-tex-server 'texlab)
	(setq lsp-enable-symbol-highlighting nil)
	(setq lsp-lens-enable nil)
	;; (setq lsp-headerline-breadcrumb-enable nil)
	(setq lsp-keymap-prefix "C-c l")
	:hook (
				 (LaTex-mode . lsp)
				 (lua-mode . lsp)
				 (lsp-mode . lsp-enable-which-key-integration)) ;whichkey-integration
	:commands lsp)

(use-package lua-mode
	:straight t)

;; optionally
(use-package lsp-ui
	:straight t
	:commands lsp-ui-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package corfu
	;; Optional customizations
	:custom
	(corfu-history-mode)
	(corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
	(corfu-auto t)                 ;; Enable auto completion
	(corfu-separator ?\s)          ;; Orderless field separator
	(corfu-quit-at-boundary 'separator)   ;; Never quit at completion boundary
	(corfu-quit-no-match 'separator)      ;; Never quit, even if there is no match
	;; (corfu-preview-current nil)    ;; Disable current candidate preview
	(corfu-preselect 'valid)      ;; Preselect the prompt
	;; (corfu-oN-exact-match nil)     ;; Configure handling of exact matches
	;; (corfu-scroll-margin 5)        ;; Use scroll margin
	(defun orderless-fast-dispatch (word index total)
		(and (= index 0) (= total 1) (length< word 4)
				 `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

	(orderless-define-completion-style orderless-fast
		(orderless-style-dispatchers '(orderless-fast-dispatch))
		(orderless-matching-styles '(orderless-literal orderless-regexp)))

	(setq-local corfu-auto t
							corfu-auto-delay 0
							corfu-auto-prefix 0
							completion-styles '(orderless-fast))
	:bind
	(:map corfu-map
				("TAB" . corfu-next)
				([tab] . corfu-next)
				("S-TAB" . corfu-previous)
				([backtab] . corfu-previous))
	:init
	(global-corfu-mode))

(use-package cape
	:init
	(add-to-list 'completion-at-point-functions #'cape-dabbrev)
	(add-to-list 'completion-at-point-functions #'cape-file)
	(add-to-list 'completion-at-point-functions #'cape-elisp-block)
	;;(add-to-list 'completion-at-point-functions #'cape-history)
	;;(add-to-list 'completion-at-point-functions #'cape-keyword)
	;;(add-to-list 'completion-at-point-functions #'cape-tex)
	;;(add-to-list 'completion-at-point-functions #'cape-sgml)
	;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
	(add-to-list 'completion-at-point-functions #'cape-abbrev)
	;;(add-to-list 'completion-at-point-functions #'cape-dict)
	;;(add-to-list 'completion-at-point-functions #'cape-symbol)
	;;(add-to-list 'completion-at-point-functions #'cape-line)
	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package solaire-mode
	:straight t)
(solaire-global-mode +1)

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
	(setq dashboard-banner-logo-title "Welcome Back Goblin")
	;; Content is not centered by default. To center, set
	(setq dashboard-startup-banner "~/.config/emacs/themes/logo.txt")
	(setq dashboard-center-content t)
	(setq dashboard-set-heading-icons t)
	(setq dashboard-set-file-icons t)
	(setq dashboard-set-navigator t)
	(setq dashboard-items '((recents  . 5)
													(bookmarks . 5)))
	;; (projects . 5)
	;; (agenda . 5)
	;; (registers . 5)))
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

(use-package pandoc-mode
	:config
	(add-hook 'markdown-mode-hook 'pandoc-mode))

;;;;;;;;;;;
;; Latex ;;
;;;;;;;;;;;

(use-package tex
	:straight auctex
	:config
	(setq TeX-save-query nil)
	(setq TeX-clean-confirm nil)
	(setq TeX-source-correlate-method 'synctex)
	(TeX-source-correlate-mode 1)
	(setq TeX-source-correlate-start-server t)

	(add-to-list 'TeX-view-program-selection
							 '(output-pdf "Zathura"))
	:hook
	(add-hook 'LaTeX-mode-hook 'prettify-symbols-mode))

(evil-define-key 'normal LaTeX-mode-map
	(kbd "<leader>tm") 'reftex-toc
	(kbd "<leader>tt") 'lsp-ui-imenu)

(use-package laas
	:straight (laas :type git :host github :repo "Stefanomarton/LaTeX-auto-activating-snippets")
	:hook (LaTeX-mode . laas-mode)
	:hook (markdown-mode . laas-mode)
	:config ; do whatever here
	(aas-set-snippets 'laas-mode
		"dm" (lambda () (interactive)
					 (yas-expand-snippet "\\[ \n $1 \n \\] $0"))
		"mk" (lambda () (interactive)
					 (yas-expand-snippet "\\\\( $1 \\\\) $0"))
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
		"inti" (lambda () (interactive)
						 (yas-expand-snippet "\\int"))
		"intd" (lambda () (interactive)
						 (yas-expand-snippet "\\int_{$1}^{$2} $0"))
		"df" (lambda () (interactive)
					 (yas-expand-snippet "_{$1}$0"))
		"rt" (lambda () (interactive)
					 (yas-expand-snippet "^{$1}$0"))
		;; add accent snippets
		:cond #'laas-object-on-left-condition
		"qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

(use-package latex-table-wizard)

(defun some-useful-name (stuff-to-configure)
	"Some useful documentation here!."
	(dolist (entry stuff-to-configure)
		(add-to-list 'latex-table-wizard-transient-keys
								 (cons (intern (concat "latex-table-wizard-" (symbol-name (car entry))))
											 (cdr entry)))))

;; example use
(some-useful-name '((right . "l")
										(left . "h")
										(beginning-of-cell . "ii")
										(down . "j")
										(up . "k")
										(end-of-cell . "a")
										(beginning-of-row . "II")
										(end-of-row . "A")
										(bottom . "G")
										(top . "gg")
										(mark-cell . "m")
										(insert-column . "C")
										(insert-row .	"R")
										(kill-column-content ."DCC"	)
										(kill-row-content . "DRC"	)
										(delete-column . "Dc"	)
										(delete-row . "Dr"	)
										))

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
(setq org-format-latex-options (plist-put org-format-latex-options :scale 1.6))
(plist-put org-format-latex-options :justify 'center)

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
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Terminal specific configuration

(if (display-graphic-p)
		;; GUI mode
		(progn
			(nyan-mode 1))
	;; Terminal mode
	())

(use-package org-modern
	:hook
	(add-hook 'org-mode-hook #'org-modern-mode))

(use-package obsidian
	:ensure t
	:demand t
	:config
	(obsidian-specify-path "~/GoogleDrive/Obsidian")
	(global-obsidian-mode t)
	;; (evil-define-key 'normal 'global (kbd "<escape>") 'evil-ex-nohighlight)
	:custom
	;; This directory will be used for `obsidian-capture' if set.
	(obsidian-inbox-directory "Inbox"))

;; Configure Tempel
(use-package tempel
	;; Require trigger prefix before template name when completing.
	;; :custom
	;; (tempel-trigger-prefix "<")
	:bind (("<leader>nc" . tempel-complete) ;; Alternative tempel-expand
				 ("C-c i" . tempel-insert))
	:init
	;; Setup completion at point
	(defun tempel-setup-capf ()
		;; Add the Tempel Capf to `completion-at-point-functions'.
		;; `tempel-expand' only triggers on exact matches. Alternatively use
		;; `tempel-complete' if you want to see all matches, but then you
		;; should also configure `tempel-trigger-prefix', such that Tempel
		;; does not trigger too often when you don't expect it. NOTE: We add
		;; `tempel-expand' *before* the main programming mode Capf, such
		;; that it will be tried first.
		(setq-local completion-at-point-functions
								(cons #'tempel-expand
											completion-at-point-functions)))

	(add-hook 'prog-mode-hook 'tempel-setup-capf)
	(add-hook 'text-mode-hook 'tempel-setup-capf)

	;; Optionally make the Tempel templates available to Abbrev,
	;; either locally or globally. `expand-abbrev' is bound to C-x '.
	(add-hook 'prog-mode-hook #'tempel-abbrev-mode)
	(global-tempel-abbrev-mode)
	)

(use-package tempel-collection)

(use-package gptel)

(use-package format-all
	:init
	(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
	(add-hook 'prog-mode-hook 'format-all-mode)
	(add-hook 'LaTeX-mode-hook 'format-all-mode)
	)
