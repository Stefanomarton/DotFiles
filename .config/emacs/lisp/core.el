(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package esup
  :commands esup)

(use-package evil
  :straight t
  :init
  (setq evil-want-keybinding nil)
  :config
  (setq evil-want-integration nil)
  (setq evil-echo-state nil)
  (setq evil-respect-visual-line-mode t)
  (setq evil-want-empty-ex-last-command t)
  (setq evil-want-C-u-scroll t) ;; allow scroll up with 'C-u'
  (setq evil-want-C-d-scroll nil) ;; avoid scroll down with 'C-d'
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-undo-system 'undo-fu)
  (setq evil-search-module 'evil-search)
  (evil-define-key 'normal 'global (kbd "C-d") 'evil-scroll-down)
  (defun toggle-transparency ()
    (interactive)
    (let ((alpha (frame-parameter nil 'alpha)))
      (set-frame-parameter
       nil 'alpha
       (if (eql (cond ((numberp alpha) alpha)
                      ((numberp (cdr alpha)) (cdr alpha))
                      ;; Also handle undocumented (<active> <inactive>) form.
                      ((numberp (cadr alpha)) (cadr alpha)))
		100)
           '(95 . 50) '(100 . 100)))))
  (evil-define-key 'normal 'global (kbd "<leader> a") 'toggle-transparency)
  (evil-mode 1))

(use-package undo-fu
  :after evil
  :straight t)

;; (use-package undo-tree
;;   :demand t
;;   :config
;;   (global-undo-tree-mode))

(use-package evil-collection
  :demand t
  :requires evil
  :after evil
  :config
  (evil-collection-init))

(use-package evil-tex
  :commands (LaTeX-mode)
  :after evil
  :hook
  (LaTeX-mode . evil-tex-mode)
  )

(use-package evil-embrace
  :requires evil
  :after evil
  :custom
  (setq evil-embrace-evil-surround-keys (?\( ?\[ ?\{ ?\) ?\] ?\} ?\" ?\' ?< ?> ?b ?B ?t))
  :config
  (setq evil-embrace-show-help-p nil)
  (evil-embrace-enable-evil-surround-integration)
  )

(use-package embrace
  :custom
  (embrace-show-help-p nil)
  :config
  (evil-define-key 'normal 'global (kbd ",") 'evil-surround-edit)
  (evil-define-key 'normal 'global (kbd "e c") 'evil-embrace-evil-surround-change)
  (evil-define-key 'normal 'global (kbd "e d") 'evil-embrace-evil-surround-delete)
  (evil-define-key 'visual 'global (kbd "e") 'evil-embrace-evil-surround-region))

(use-package evil-surround
  :after evil
  :config
  (add-hook 'prog-mode-hook (lambda ()
                              (push '(?\( . ("\(" . "\)")) evil-surround-pairs-alist)))
  )

(use-package evil-commentary
  ;; Better Comment Action
  :requires evil
  :after evil
  :config
  (evil-define-key 'visual 'global (kbd "gc") 'evil-commentary)
  (evil-define-key 'normal 'global (kbd "gcA") 'comment_end_of_line)
  (evil-define-key 'visual 'global (kbd "gb") 'comment-box)
  (evil-define-key 'normal 'global (kbd "gcc") 'evil-commentary-line))

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
(evil-define-key 'normal 'global (kbd "C-u") 'evil-scroll-up)
(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'visual (kbd "SPC"))

;; Offer to diff buffer when closing it
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

(use-package consult
  :defer 1)

(use-package consult-projectile
  :after (consult projectile))

(use-package consult-dir
  :after consult)

(use-package consult-flycheck
  :after (consult flycheck))

(use-package consult-flyspell
  :after consult)

;; Enable vertico
(use-package vertico
  :defer t
  :bind
  (:map vertico-map
	("C-e" . embark-minimal-act)
	("C-j" . vertico-next)
	("C-k" . vertico-previous)
	("<escape>" . keyboard-escape-quit))
  :config
  ;; Different scroll margin
  (setq vertico-scroll-margin 0)
  ;; Show more candidates
  (setq vertico-count 10)
  (setq vertico-resize nil)
  (setq vertico-cycle t)
  :init
  (vertico-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(use-package recentf
  :config
  (add-hook 'emacs-startup-hook 'recentf-mode)
  (add-hook 'after-init-hook
            (lambda ()
	      (setq inhibit-message t)
	      (run-with-idle-timer 0 nil (lambda () (setq inhibit-message nil)))))
  (setq recentf-auto-cleanup 'never)
  (setq recentf-max-saved-items 25))

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
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :defer 1
  :config
  (marginalia-mode))

(use-package embark
  :after marginalia
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
  :demand
  :bind (:map embark-become-file+buffer-map
	      ("m" . consult-bookmark)
	      ("b" . consult-buffer)
	      ("j" . consult-find)))

(defun smart-for-files ()
  (interactive)
  (if (projectile-project-p)
      (consult-projectile-find-file)
    (call-interactively #'find-file)))

(defun smart-for-buffer ()
  (interactive)
  (if (projectile-project-p)
      (consult-projectile-switch-to-buffer)
    (consult-buffer)))

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

;;Settings normal global keybindings
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
  (kbd "<leader>ee") 'eval-buffer
  (kbd "<leader>es") 'eval-expression
  (kbd "<leader>er") 'eval-region
  (kbd "<leader>ef") 'eval-defun
  (kbd "<leader>pp") 'consult-projectile
  (kbd "<leader>ps") 'consult-projectile-switch-project
  (kbd "<leader>cc") 'calc
  (kbd "<leader>qc") 'quick-calc
  (kbd "<leader>t") 'smart-for-terminal-otherw
  (kbd "<leader>T") 'smart-for-terminal
  (kbd "<leader>gg") 'magit-status-with-removed-dotfiles-args
  (kbd "<leader>gd") 'dotfiles-magit-status
  )

(evil-define-key 'insert 'global (kbd "C-<backspace>") 'evil-delete-backward-word)
(evil-define-key 'visual 'global (kbd "<leader>gg") 'google-this-noconfirm)
(evil-define-key 'normal 'prog-mode-map (kbd "<leader>m") 'rainbow-mode)

(use-package evil-goggles
  :straight t
  :after evil
  :config
  (evil-goggles-mode)
  (setq evil-goggle-duration 0.5)
  (evil-goggles-use-diff-faces))

(defun comment_end_of_line ()
  (interactive)
  (call-interactively 'comment-dwim)
  (call-interactively 'evil-append))

(define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

(use-package dired
  :commands dired
  :straight nil
  :ensure nil
  :config
  (evil-define-key 'normal dired-mode-map
    (kbd "f") 'dired-narrow-fuzzy
    (kbd "T") 'dired-create-empty-file
    (kbd "<RET>") 'dired-find-alternate-file
    (kbd "<escape>") 'keyboard-escape-quit
    (kbd "h") 'dired-up-directory))

(use-package dired-narrow
  :config
  (defun dired-narrow-ex-ac ()
    ;; Revert buffer and enter the directory after narrowing
    (revert-buffer)
    (dired-find-alternate-file))
  (setq dired-narrow-exit-when-1-left t)
  (setq dired-narrow-exit-action 'dired-narrow-ex-ac)
  )

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

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

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
  :config
  (setq fast-but-imprecise-scrolling t
	jit-lock-defer-time 0)
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

(use-package doom-modeline
  :custom
  (inhibit-compacting-font-caches t)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-height 15)
  (doom-modeline-hud t)
  :init
  (doom-modeline-mode 1))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  ;; (setq max-specpdl-size 64000) ;; HACK - fix bug in LSP
  (setq lsp-tex-server 'texlab)
  (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-lens-enable nil)
  (setq lsp-completion-provider :none) ;; must have to make yasnippet backend work correctly
  (setq lsp-completion-enable t)
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode)
  :hook
  ((lua-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)
   (LaTeX-mode . lsp)
   (python-mode . lsp)) ;whichkey-integration
  )

(use-package lsp-ui
  :hook
  (lsp-mode . lsp-ui-mode)
  :straight t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-latex
  :commands (latex-mode)
  :straight (lsp-latex :type git :host github :repo "ROCKTAKEY/lsp-latex")
  :config
  (setq lsp-latex-build-forward-search-after nil)
  (setq lsp-latex-build-executable "tectonic")
  (setq lsp-latex-forward-search-executable "zathura")
  (setq lsp-latex-forward-search-args '("--synctex-forward" "%l:1:%f" "%p"))
  (setq lsp-latex-build-on-save nil)
  (setq lsp-latex-build-args '("-X" "compile" "%f" "--synctex")))

(use-package lua-mode
  :defer t
  :straight t)


;; Editorconfig, auto set indenting
(use-package editorconfig
  :defer 1
  :config
  (editorconfig-mode 1))

(use-package electric
  :straight (:type built-in)
  :init
  (electric-pair-mode +1) ;; automatically insert closing parens
  (setq electric-pair-preserve-balance nil)) ;; more annoying than useful

;; Highlight nested parentheses
(use-package rainbow-delimiters
  :defer 1
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
		      :foreground "red"
		      :inherit 'error
		      :box t))

;; Highlight colorstring with the right color
(use-package rainbow-mode
  :commands rainbow-mode
  :config
  (add-hook 'prog-mode #'rainbow-mode))

(use-package dashboard
  :ensure t
  :config
  (evil-define-key 'normal dashboard-mode-map
    (kbd "f") 'find-file
    (kbd "h") 'consult-projectile-switch-project
    (kbd "c") (lambda ()
                (interactive)
                (let ((folder-path "~/.config/emacs"))
                  (find-file folder-path))))

  :custom
  (initial-buffer-choice #'(lambda () (get-buffer-create "*dashboard*")))
  (dashboard-banner-logo-title "Welcome Back Goblin")
  ;; Content is not centered by default. To center, set
  (dashboard-startup-banner "~/.config/emacs/themes/logo.txt")
  (dashboard-center-content t)
  (dashboard-set-footer nil)
  (dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
  ;; (setq dashboard-set-navigator t)
  ;; (setq dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-items nil)
  ;; (dashboard-items '((recents  . 10)))
  ;; (bookmarks . 5))
  ;; (projects . 5)))
  ;; (agenda . 5)
  ;; (registers . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package google-this
  :defer t)

(use-package org-bullets
  :defer t
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
  ;; (add-hook 'org-mode-hook 'visual-line-mode)
  (add-hook 'org-mode-hook 'org-indent-mode)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(defun efs/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . efs/org-mode-visual-fill))

(use-package tab-jump-out
  :custom
  (tab-jump-out-mode 1))

;; (use-package things
;;   :straight (things :type git :host github
;; 		    :repo "noctuid/things.el"))

;; (use-package nyan-mode)

(use-package org-download
  :after org-mode
  :hook
  (add-hook 'dired-mode-hook 'org-download-enable))

(defun export-buffer-to-pdf ()
  "Export current buffer to PDF using Pandoc asynchronously without minibuffer output."
  (interactive)
  (let* ((output-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf"))
	 (process-buffer (generate-new-buffer "*pandoc-export*"))
	 (exit-code (call-process "pandoc" nil process-buffer nil
				  (buffer-file-name) "-o" output-file "-V" "geometry:margin=20mm" "--template=template.latex" "--pdf-engine=xelatex")))
    (if (zerop exit-code)
	(message "Exported to %s" output-file)
      (with-current-buffer process-buffer
	(message "Export failed: %s" (buffer-string))))
    (kill-buffer process-buffer)))

(defun open-pdf-with-zathura ()
  "Open the PDF file associated with the current buffer in Zathura."
  (interactive)
  (let ((pdf-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
    (start-process "zathura" nil "zathura" pdf-file)))

(defun open-pdf-with-pdf-tools ()
  "Open the PDF file associated with the current buffer in pdf-tools."
  (interactive)
  (let ((pdf-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
    (if (file-exists-p pdf-file)
	(progn
	  (pdf-tools-install)
	  (find-file pdf-file))
      (message "PDF file not found."))))

;; Auto reload pdf and suppress messages
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

(use-package pdf-tools
  :after (latex-mode markdown-mode org-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page) ; Fit page width
  (setq pdf-annot-activate-created-annotations t) ; Enable annotations
  (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "h") 'image-backward-hscroll)
  (define-key pdf-view-mode-map (kbd "l") 'image-forward-hscroll)
  (define-key pdf-view-mode-map (kbd "g") 'pdf-view-first-page)
  (define-key pdf-view-mode-map (kbd "G") 'pdf-view-last-page)
  (define-key pdf-view-mode-map (kbd "C-j") 'pdf-view-next-page)
  (define-key pdf-view-mode-map (kbd "C-k") 'pdf-view-previous-page)
  (define-key pdf-view-mode-map (kbd "C-d") 'pdf-view-scroll-up-or-next-page)
  (define-key pdf-view-mode-map (kbd "C-u") 'pdf-view-scroll-down-or-previous-page)
  )

(use-package markdown-mode
  :config
  (evil-define-key 'normal markdown-mode-map
    (kbd "<leader>ee") 'export-buffer-to-pdf
    (kbd "<leader>ez") 'open-pdf-with-zathura
    (kbd "<leader>ep") 'open-pdf-with-pdf-tools)
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-enable-math t))
;; :init (setq markdown-command "multimarkdown"))

;;;;;;;;;;;
;; Latex ;;
;;;;;;;;;;;

(use-package tex
  :commands latex-mode
  :straight auctex
  :config
  (setq TeX-save-query nil
	TeX-clean-confirm nil
	TeX-command-default "XeLaTeX"
	TeX-source-correlate-start-server t
	TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-mode 1)
  (add-to-list 'TeX-view-program-selection
	       '(output-pdf "Zathura"))

  (defun my-export-to-pdf ()
    "Export the current LaTeX document to PDF using AUCTeX."
    (interactive)
    (TeX-command "LaTeX" 'TeX-master-file nil)
    (TeX-clean))

  (defun my-export-to-pdf-and-view ()
    "Export the current LaTeX document to PDF using AUCTeX."
    (interactive)
    (TeX-command "LaTeX" 'TeX-master-file nil)
    (TeX-clean)
    (TeX-view)
    )

  ;; Toggle between master and current compilation
  (defvar my-latex-original-master nil
    "Variable to store the original value of TeX-master.")

  (defun my-latex-toggle-command ()
    "Toggle between executing commands on master and current file."
    (interactive)
    (if my-latex-original-master
	(progn
	  (setq TeX-master my-latex-original-master)
	  (setq my-latex-original-master nil))
      (progn
	(setq my-latex-original-master TeX-master)
	(setq TeX-master nil)))
    (message "Switched command: %s" (if TeX-master "master" "current")))

  (evil-define-key 'normal LaTeX-mode-map
    (kbd "C-c e") 'my-export-to-pdf
    (kbd "C-c T") 'my-latex-toggle-command
    (kbd "C-c E") 'my-export-to-pdf-view
    (kbd "C-c t") 'lsp-ui-imenu)

  :init
  (add-hook 'LaTeX-mode-hook 'prettify-symbols-mode))

(use-package yasnippet
  :hook
  (prog-mode . yas-minor-mode)
  (LaTeX-mode . yas-minor-mode)
  (markdown-mode . yas-minor-mode)
  (org-mode . yas-minor-mode)
  :commands (yas-minor-mode))

(use-package aas
  :commands (org-mode markdown-mode latex-mode)
  :hook
  (org-mode . aas-activate-for-major-mode)
  (markdown-mode . aas-activate-for-major-mode)
  (LaTeX-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'latex-mode
		    "jf" (lambda () (interactive)
			   (yas-expand-snippet "\\\\($1\\\\) $0"))
		    "kd" (lambda () (interactive)
			   (yas-expand-snippet "\\[ \n $1 \n \\] \n \n $0")))
  (aas-set-snippets 'org-mode
		    "jf" (lambda () (interactive)
			   (yas-expand-snippet "\\\\( $1 \\\\) $0"))
		    "kd" (lambda () (interactive)
			   (yas-expand-snippet "\\[ \n $1 \n \\] \n \n $0")))
  (aas-set-snippets 'markdown-mode
		    "jf" (lambda () (interactive)
			   (yas-expand-snippet "$$1$ $0"))
		    "kd" (lambda () (interactive)
			   (yas-expand-snippet "$$ \n $1 \n $$ \n \n $0"))))

(use-package laas
  :commands (org-mode markdown-mode latex-mode)
  :straight (laas :type git :host github :repo "Stefanomarton/LaTeX-auto-activating-snippets")
  :hook
  (LaTeX-mode . laas-mode)
  (markdown-mode . laas-mode)
  (org-mode . laas-mode)
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

(use-package cdlatex
  :commands latex-mode
  :hook (LaTeX-mode . cdlatex-mode)
  :custom
  (cdlatex-takeover-dollar nil)
  (cdlatex-math-modify-prefix 59))

(use-package latex-table-wizard
  :commands latex-mode
  :config
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
		      )))

;;;;;;;;;;;;;;;
;; AvyConfig ;;
;;;;;;;;;;;;;;;

(use-package avy
  :commands (prog-mode latex-mode markdown-mode org-mode)
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
(evil-define-key 'operator 'global (kbd "R") 'avy-copy-region)
;; (evil-define-key 'motion 'global (kbd "L") 'avy-copy-line)

(setq avy-timeout-seconds 0.25)

(use-package expand-region
  :commands (prog-mode org-mode latex-mode markdown-mode)
  :config
  (defun expand-region ()
    "Repeat the `er/expand-region' command."
    (interactive)
    (dotimes (_ 2)
      (call-interactively 'er/expand-region)))
  (evil-define-key 'normal 'global (kbd "C-SPC") 'expand-region)
  (setq expand-region-subword-enabled t)
  )

(use-package org-fragtog
  :commands (org-mode)
  :hook
  (org-mode . org-fragtog-mode)
  :after org-mode
  :config
  ;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.6))
  (plist-put org-format-latex-options :justify 'center)
  )

(use-package org-modern
  :commands (org-mode)
  :after org-mode
  :hook
  (org-mode-hook . org-modern-mode))

(use-package gptel
  :commands gptel)

(use-package format-all
  :init
  (add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
  (add-hook 'prog-mode-hook 'format-all-mode)
  (add-hook 'LaTeX-mode-hook 'format-all-mode)
  )

(use-package yasnippet
  :commands (yas-minor-mode) ; autoload `yasnippet' when `yas-minor-mode' is called
                                        ; using any means: via a hook or by user
                                        ; Feel free to add more commands to this
                                        ; list to suit your needs.
  :hook
  (prog-mode . yas-minor-mode)
  (laas-mode . yas-minor-mode)
  :config ; stuff to do after requiring the package
  (setq yas-snippet-dirs '("~/.config/emacs/snippets"))
  (progn
    (yas-reload-all)))

(use-package projectile
  :diminish projectile-mode
  :commands
  (consult-projectile consult-project-buffer consult-projectile-recentf consult-projectile-find-dir consult-projectile-find-file consult-projectile-switch-project consult-projectile-switch-to-buffer)
  :custom
  (setq projectile-enable-caching t)
  (setq projectile-track-known-projects-automatically nil)
  (setq projectile-completion-system 'consult)
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  :init
  (setq projectile-known-projects-file "~/.config/emacs/project.el")
  (setq projectile-indexing-method 'native)
  (projectile-mode))

(use-package magit
  :commands (magit-status magit-file-dispatch magit-dispatch dotfiles-magit-status magit-status-with-removed-dotfiles-args)
  :config
  (magit-auto-revert-mode)
  (setq magit-commit-ask-to-stage 'stage)
  ;; prepare the arguments
  (setq dotfiles-git-dir (concat "--git-dir=" (expand-file-name "~/.dotfiles")))
  (setq dotfiles-work-tree (concat "--work-tree=" (expand-file-name "~")))

  ;; function to start magit on dotfiles
  (defun dotfiles-magit-status ()
    (interactive)
    (add-to-list 'magit-git-global-arguments dotfiles-git-dir)
    (add-to-list 'magit-git-global-arguments dotfiles-work-tree)
    (call-interactively 'magit-status))

  ;; wrapper to remove additional args before starting magit
  (defun magit-status-with-removed-dotfiles-args ()
    (interactive)
    (setq magit-git-global-arguments (remove dotfiles-git-dir magit-git-global-arguments))
    (setq magit-git-global-arguments (remove dotfiles-work-tree magit-git-global-arguments))
    (call-interactively 'magit-status))

  ;; Fixing keybinding
  (evil-define-key 'normal magit-mode-map (kbd "h") 'magit-section-backward-sibling)
  (evil-define-key 'normal magit-mode-map (kbd "l") 'magit-section-forward-sibling)
  (evil-define-key 'normal magit-mode-map (kbd "SPC") 'magit-section-cycle)
  )

(use-package magit-delta
  :after magit
  :commands magit-delta-mode
  :hook (magit-mode . magit-delta-mode))

(use-package vterm
  :commands vterm
  :config
  ;; (set-fontset-font t 'unicode (font-spec :family "JetBrainsMono Nerd Font"))
  :custom
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"
	vterm-internal-use-ligatures t
	vterm-max-scrollback 10000
	vterm-shell "zsh"
	))

;; Python mode setup
(use-package python-mode
  :commands python-mode
  :straight (:type built-in)
  :mode ".py"
  :interpreter "python"
  :init
  (evil-define-key 'normal python-mode-map (kbd "<tab>") 'evil-shift-right-line)
  (evil-define-key 'normal python-mode-map (kbd "<backtab>") 'evil-shift-left-line)
  (evil-define-key 'visual python-mode-map (kbd "<tab>") 'evil-shift-right)
  (evil-define-key 'visual python-mode-map (kbd "<backtab>") 'evil-shift-left)
  )

(use-package flycheck
  :commands (flycheck-mode)
  :defer t
  )

(use-package csv-mode
  :commands csv-mode
  )

(use-package hl-todo
  :defer t
  :init
  (global-hl-todo-mode))

(use-package git-gutter
  :hook ((prog-mode markdown-mode LaTeX-mode) . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :config
  (fringe-mode nil)
  (setq-default left-margin-width 1)
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [224] nil nil '(center repeated)))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :commands (dired-jump dirvish-dwim)
  :config
  (evil-define-key 'normal dirvish-mode-map (kbd "q") 'dirvish-quit)
  (evil-define-key 'normal dirvish-mode-map (kbd "<escape>") 'dirvish-layout-toggle)
  )

(use-package langtool
  :config
  (setq langtool-java-classpath
	"/usr/share/languagetool:/usr/share/java/languagetool/*"))

(use-package flycheck-languagetool
  :after flycheck
  :requires flyckeck langtool
  :init
  (setq flycheck-languagetool-server-jar "~/Downloads/LanguageTool-6.1/languagetool-server.jar"))

;; A more complex, more lazy-loaded config
(use-package solaire-mode
  :defer 1
  :hook
  ;; Ensure solaire-mode is running in all solaire-mode buffers
  (change-major-mode . turn-on-solaire-mode)
  ;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
  ;; itself off every time Emacs reverts the file
  (after-revert . turn-on-solaire-mode)
  ;; To enable solaire-mode unconditionally for certain modes:
  (ediff-prepare-buffer . solaire-mode)
  :custom
  (solaire-mode-auto-swap-bg t)
  :config
  (solaire-global-mode +1))

(provide 'core)
;; core.el ends here
