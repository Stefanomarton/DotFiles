;; Makes *scratch* empty:
(setq initial-scratch-message "")

					; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

					; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
					; (add-hook 'minibuffer-exit-hook
					;       '(lambda ()
					;          (let ((buffer "*Completions*"))
					;           (and (get-buffer buffer)
					;                 (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; System clipboard
(setq select-enable-clipboard t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message t) ;; Don't show startup messages
(scroll-bar-mode -1) ;; No scrollbar
(tool-bar-mode -1) ;; No top bar
(tooltip-mode -1) ;; No tooltip
(set-fringe-mode 10)

(menu-bar-mode -1)
(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 150)
(load-theme 'doom-nord t)
;;(setq custom-safe-themes t)

(column-number-mode)
(global-display-line-numbers-mode 1)
(setq display-line-numbers 'relative)

(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")
			 ("elpa" . "https://elpa.gnu.org/packages/")))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "S-C-c") 'kill-ring-save)
(global-set-key (kbd "S-C-v") 'yank)
(global-set-key (kbd "K") nil)
(global-set-key (kbd "Y") nil)

(use-package general
  :config
  (general-create-definer rune/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (rune/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")
    "f" '(counsel-find-file :which-key "Find files")
    "p" '(counsel-M-x :which-key "Command center")
    "q" '(kill-buffer-and-window :which-key "Close")
    "w" '(counsel-ibuffer :which-key "Navigate buffers")
    "e" '(eval-buffer :which-key "eval buffer")
    "ggl" '(google-this-noconfirm :which-key "Google the selection")))

;; Revome useless files and keep folders clean
(setq user-emacs-directory "~/.cache/emacs")
(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq create-lockfiles nil)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(use-package ivy
  :diminish
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file))
  :config
  (setq ivy-initial-inputs-alist nil)) ;; Dont't start searches with ^

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

(use-package doom-themes)

;; Enable Evil
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-undo-system 'undo-tree)
  (setq evil-search-module 'evil-search)
  (evil-mode 1))

;; nvim surround functionalities https://github.com/emacs-evil/evil-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-nerd-commenter)

;;Undo package
(use-package undo-tree
  :ensure t
  :config
  (global-undo-tree-mode 1))

(use-package all-the-icons
  :ensure t)

;; Which Key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; Rainbow Delimeters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))


;;(use-package Smartparens)

;; enable smartparens mode
;;(smartparens-global-mode t)

(use-package tab-jump-out)
(tab-jump-out-mode)
					;(setq yas-fallback-behavior '(apply tab-jump-out 1))

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-H" . centaur-tabs-backward)
  ("C-L" . centaur-tabs-forward))

(use-package evil-collection)
(evil-collection-init)

;; Enable indent guide lines https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

(use-package indent-guide
  :config (indent-guide-global-mode))


(use-package evil-tex)
(add-hook 'LaTeX-mode-hook #'evil-tex-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" default))
 '(package-selected-packages
   '(helpful format-all dashboard google-this flycheck no-littering org-table preview evil-tex evil-surround cdlatex highlight-indent-guides latex-preview-pane lua-mode yasnippet auto-complete evil-collection evil-collections centaur-tabs tab-jump-out smartparens consult treesitter general lsp-mode undo-tree auctex doom-themes doom-theme all-the-icons evil doom-modeline command-log-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; AucTeX settings - almost no changes
(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . prettify-symbols-mode))
  :bind (:map LaTeX-mode-map
              ("C-S-e" . latex-math-from-calc))
  :config
  ;; Format math as a Latex string with Calc
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0)
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad))))))))

(setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist)) ; Enable Latex-Mode when entry in a .tex file


;; CDLatex settings
;; (use-package cdlatex
;;   :ensure t
;;   :hook (LaTeX-mode . turn-on-cdlatex)
;;   :bind (:map cdlatex-mode-map
;;               ("<tab>" . cdlatex-tab)))

;; Yasnippet settings
(use-package yasnippet
  :ensure t
  :hook ((LaTeX-mode . yas-minor-mode)
         (post-self-insert . my/yas-try-expanding-auto-snippets))
  :config
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

(setq yas-snippet-dirs
      '("~/.config/emacs/yasnippets"                 ;; personal snippets
        ))

(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.
					;(yas-reload-all)
					;(add-hook 'prog-mode-hook #'yas-minor-mode)

;; CDLatex integration with YaSnippet: Allow cdlatex tab to work inside Yas
;; fields
;; (use-package cdlatex
;;   :hook ((cdlatex-tab . yas-expand)
;;          (cdlatex-tab . cdlatex-in-yas-field))
;;   :config
;;   (use-package yasnippet
;;     :bind (:map yas-keymap
;; 		("<tab>" . yas-next-field-or-cdlatex)
;; 		("TAB" . yas-next-field-or-cdlatex)
;; 		("<backtab>" . yas-prev-field))
;;     :config
;;     (defun cdlatex-in-yas-field ()
;;       ;; Check if we're at the end of the Yas field
;;       (when-let* ((_ (overlayp yas--active-field-overlay))
;;                   (end (overlay-end yas--active-field-overlay)))
;;         (if (>= (point) end)
;;             ;; Call yas-next-field if cdlatex can't expand here
;;             (let ((s (thing-at-point 'sexp)))
;;               (unless (and s (assoc (substring-no-properties s)
;;                                     cdlatex-command-alist-comb))
;;                 (yas-next-field-or-maybe-expand)
;;                 t))
;;           ;; otherwise expand and jump to the correct location
;;           (let (cdlatex-tab-hook minp)
;;             (setq minp
;;                   (min (save-excursion (cdlatex-tab)
;;                                        (point))
;;                        (overlay-end yas--active-field-overlay)))
;;             (goto-char minp) t))))

;;     (defun yas-next-field-or-cdlatex nil
;;       (interactive)
;;       "Jump to the next Yas field correctly with cdlatex active."
;;       (if
;;           (or (bound-and-true-p cdlatex-mode)
;;               (bound-and-true-p org-cdlatex-mode))
;;           (cdlatex-tab)
;;         (yas-next-field-or-maybe-expand)))))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; LSP mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-modeline-diagnostics-enable))

(use-package volatile-highlights
  :config

  (volatile-highlights-mode t)
  (vhl/define-extension 'undo-tree 'undo-tree-yank 'undo-tree-move)
  (vhl/install-extension 'undo-tree)

  (vhl/define-extension 'evil 'evil-paste-after 'evil-paste-before 'evil-paste-pop 'evil-move 'evil-yank 'evil-yank-line)
  (vhl/install-extension 'evil)
  )

(use-package google-this
  :config (google-this-mode 1))

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook))
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

(use-package format-all)
(format-all-mode 1)

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



; Array/tabular input with org-tables and cdlatex
  ;;      (use-package org-table
  ;;      :after cdlatex
  ;;      :bind (:map orgtbl-mode-map
  ;;             ("<tab>" . lazytab-org-table-next-field-maybe)
  ;;             ("TAB" . lazytab-org-table-next-field-maybe))
  ;; :init
  ;; (add-hook 'cdlatex-tab-hook 'lazytab-cdlatex-or-orgtbl-next-field 90)
  ;; ;; Tabular environments using cdlatex
  ;; (add-to-list 'cdlatex-command-alist '("smat" "Insert smallmatrix env"
  ;;                                      "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
  ;;                                      lazytab-position-cursor-and-edit
  ;;                                      nil nil t))
  ;; (add-to-list 'cdlatex-command-alist '("bmat" "Insert bmatrix env"
  ;;                                      "\\begin{bmatrix} ? \\end{bmatrix}"
  ;;                                      lazytab-position-cursor-and-edit
  ;;                                      nil nil t))
  ;; (add-to-list 'cdlatex-command-alist '("pmat" "Insert pmatrix env"
  ;;                                      "\\begin{pmatrix} ? \\end{pmatrix}"
  ;;                                      lazytab-position-cursor-and-edit
  ;;                                      nil nil t))
  ;; (add-to-list 'cdlatex-command-alist '("tbl" "Insert table"
  ;;                                       "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
  ;;                                      lazytab-position-cursor-and-edit
  ;;                                      nil t nil))
  ;; :config
  ;; ;; Tab handling in org tables
  ;; (defun lazytab-position-cursor-and-edit ()
  ;;   ;; (if (search-backward "\?" (- (point) 100) t)
  ;;   ;;     (delete-char 1))
  ;;   (cdlatex-position-cursor)
  ;;   (lazytab-orgtbl-edit))

  ;; (defun lazytab-orgtbl-edit ()
  ;;   (advice-add 'orgtbl-ctrl-c-ctrl-c :after #'lazytab-orgtbl-replace)
  ;;   (orgtbl-mode 1)
  ;;   (open-line 1)
  ;;   (insert "\n|"))

  ;; (defun lazytab-orgtbl-replace (_)
  ;;   (interactive "P")
  ;;   (unless (org-at-table-p) (user-error "Not at a table"))
  ;;   (let* ((table (org-table-to-lisp))
  ;;          params
  ;;          (replacement-table
  ;;           (if (texmathp)
  ;;               (lazytab-orgtbl-to-amsmath table params)
  ;;             (orgtbl-to-latex table params))))
  ;;     (kill-region (org-table-begin) (org-table-end))
  ;;     (open-line 1)
  ;;     (push-mark)
  ;;     (insert replacement-table)
  ;;     (align-regexp (region-beginning) (region-end) "\\([:space:]*\\)& ")
  ;;     (orgtbl-mode -1)
  ;;     (advice-remove 'orgtbl-ctrl-c-ctrl-c #'lazytab-orgtbl-replace)))

  ;; (defun lazytab-orgtbl-to-amsmath (table params)
  ;;   (orgtbl-to-generic
  ;;    table
  ;;    (org-combine-plists
  ;;     '(:splice t
  ;;               :lstart ""
  ;;               :lend " \\\\"
  ;;               :sep " & "
  ;;               :hline nil
  ;;               :llend "")
  ;;     params)))

  ;; (defun lazytab-cdlatex-or-orgtbl-next-field ()
  ;;   (when (and (bound-and-true-p orgtbl-mode)
  ;;              (org-table-p)
  ;;              (looking-at "[[:space:]]*\\(?:|\\|$\\)")
  ;;              (let ((s (thing-at-point 'sexp)))
  ;;                (not (and s (assoc s cdlatex-command-alist-comb)))))
  ;;     (call-interactively #'org-table-next-field)
  ;;     t))

  ;; (defun lazytab-org-table-next-field-maybe ()
  ;;   (interactive)
  ;;   (if (bound-and-true-p cdlatex-mode)
  ;;       (cdlatex-tab)
  ;;     (org-table-next-field))))

(use-package aas
  :hook (LaTeX-mode . aas-activate-for-major-mode)
  :hook (org-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'text-mode
    ;; expand unconditionally
    ";o-" "ō"
    ";i-" "ī"
    ";a-" "ā"
    ";u-" "ū"
    ";e-" "ē")
  (aas-set-snippets 'latex-mode
    ;; set condition!
    :cond #'texmathp ; expand only while in math
    "supp" "\\supp"
    "On" "O(n)"
    "O1" "O(1)"
    "Olog" "O(\\log n)"
    "Olon" "O(n \\log n)"
    ;; bind to functions!
    "//" (lambda () (interactive)
           (yas-expand-snippet "\\frac{$1}{$2}$0"))
    "Span" (lambda () (interactive)
             (yas-expand-snippet "\\Span($1)$0")))
  ;; disable snippets by redefining them with a nil expansion
  (aas-set-snippets 'latex-mode
    "supp" nil))

(use-package laas
  :hook (LaTeX-mode . laas-mode)
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
                    "Sum" (lambda () (interactive)
                            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
                    "Span" (lambda () (interactive)
                             (yas-expand-snippet "\\Span($1)$0"))
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

(use-package helpful)
