;; Add the load path to load module of the config
(add-to-list 'load-path "~/.config/emacs/lisp/")

;; General options for emacs
(require 'options)

;; Latex configuration
(require 'latexconfig)

;; Org-mode config
(require 'orgconfig)

(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 150)
(load-theme 'doom-flatwhite t)

;; Requirin package
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
												 ("org" . "https://orgmode.org/elpa/")
												 ("elpa" . "https://elpa.gnu.org/packages/")))

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "S-C-c") 'kill-ring-save)
(global-set-key (kbd "S-C-v") 'yank)
;;(global-set-key (kbd "C-l") 'centaur-tabs-forward-tab)

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

;(evil-global-set-key 'normal (kbd "J") 'evil-next-line)
;(evil-define-key 'normal 'global (kbd "K") 'evil-previous-line)
;;(global-unset-key (kbd "K") nil)
;;(evil-define-key 'normal 'local (kbd "K") nil)
(setq evil-lookup-func nil)
(eval-after-load "evil-maps"
  '(progn
     (setq evil-lookup-func nil)))

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
  (setq evil-undo-system 'undo-fu)
  (setq evil-search-module 'evil-search)
  (evil-mode 1))

;; nvim surround functionalities https://github.com/emacs-evil/evil-surround
(use-package evil-surround)

(use-package evil-nerd-commenter
  ;; :config
  ;; (general
  ;;   "gc" 'evilnc-comment-operator
  ;;   "gC" 'evilnc-copy-and-comment-operator)
  )

(use-package evil-snipe
  :after evil
  :demand
  :config
  (evil-snipe-mode +1))
;;(evil-snipe-override-mode +1))

;;Undo package
(use-package undo-fu)

;; In case that image do not show correctly run M-x all-the-icons-install-fonts
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
  :hook (emacs-startup . centaur-tabs-mode)
  :init
  (setq centaur-tabs-set-icons t)
  (setq centaur-tabs-set-modified-marker t
        centaur-tabs-modified-marker "M"
        centaur-tabs-cycle-scope 'tabs)
  (setq centaur-tabs-set-close-button nil)
  (setq centaur-tabs-enable-ido-completion nil)
  :config
  (centaur-tabs-mode t)
  ;; (centaur-tabs-headline-match)
  )
(use-package evil-collection)
(evil-collection-init)

;; Enable indent guide lines https://github.com/DarthFennec/highlight-indent-guides
(use-package highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'bitmap)
(setq highlight-indent-guides-responsive nil)

(use-package indent-guide)

(use-package evil-tex)
(add-hook 'LaTeX-mode-hook #'evil-tex-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
	 '("4dcf06273c9f5f0e01cea95e5f71c3b6ee506f192d75ffda639d737964e2e14e" "1a1ac598737d0fcdc4dfab3af3d6f46ab2d5048b8e72bc22f50271fd6d393a00" default))
 '(package-selected-packages
	 '(lazytab ivy-postframe olivetti csv-mode restart-emacs marginalia orderless vertico centered-cursor-mode evil-goggles helpful format-all dashboard google-this flycheck no-littering org-table preview evil-tex evil-surround cdlatex highlight-indent-guides latex-preview-pane lua-mode yasnippet auto-complete evil-collection evil-collections centaur-tabs tab-jump-out smartparens consult treesitter general lsp-mode undo-tree auctex doom-themes doom-theme all-the-icons evil doom-modeline command-log-mode use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-goggles-change-face ((t (:inherit diff-removed))))
 '(evil-goggles-delete-face ((t (:inherit diff-removed))))
 '(evil-goggles-paste-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-add-face ((t (:inherit diff-added))))
 '(evil-goggles-undo-redo-change-face ((t (:inherit diff-changed))))
 '(evil-goggles-undo-redo-remove-face ((t (:inherit diff-removed))))
 '(evil-goggles-yank-face ((t (:inherit diff-changed)))))

(setq yas-snippet-dirs
      '("~/.config/emacs/snippets"                 ;; personal snippets
        ))

(yas-global-mode 1) ;; or M-x yas-reload-all if you've started YASnippet already.
																				;(yas-reload-all)
																				;(add-hook 'prog-mode-hook #'yas-minor-mode)

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

;; Displays a visual hint when editing with evil.
(use-package evil-goggles
  :after evil
  :demand
  :init
  (setq evil-goggles-duration 0.2)
  :config
  ;; (push '(evil-operator-eval
  ;;         :face evil-goggles-yank-face
  ;;         :switch evil-goggles-enable-yank
  ;;         :advice evil-goggles--generic-async-advice)
  ;;       evil-goggles--commands)
  (evil-goggles-mode)
  (evil-goggles-use-diff-faces)
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



(use-package helpful)
(use-package restart-emacs)

;; Center cursor when scrolling
;; (use-package centered-cursor-mode
;;   :general
;;   (lc/leader-keys
;;     "t =" '((lambda () (interactive) (centered-cursor-mode 'toggle)) :wk "center cursor")
;;     )
;;   )


(use-package csv-mode
  :hook (csv-mode . lc/init-csv-mode)
  :general
  (lc/local-leader-keys
   :keymaps 'csv-mode-map
   :states 'normal
   "a" '(csv-align-fields :wk "align fields")
   "A" '(lc/csv-align-visible :wk "align fields, visible")
   "i"  '(lc/init-csv-mode :wk "init csv mode")
   "u" '(csv-unalign-fields :wk "unalign fields")
   "s" '(csv-sort-fields :wk "sort fields")
   ";" '(lc/set-csv-semicolon-separator :wk "set semicolon sep")
   "," '(lc/reset-csv-separators :wk "set comma sep"))
  :init
  (defun lc/csv-align-visible (&optional arg)
    "Align visible fields"
    (interactive "P")
    (csv-align-fields nil (window-start) (window-end)))
  (defun lc/set-csv-semicolon-separator ()
    (interactive)
    (customize-set-variable 'csv-separators '(";")))
  (defun lc/reset-csv-separators ()
    (interactive)
    (customize-set-variable 'csv-separators lc/default-csv-separators))
  (defun lc/init-csv-mode ()
    (interactive)
    (lc/set-csv-separators)
    (lc/csv-highlight)
    (call-interactively 'csv-align-fields))
  :config
  (require 'cl)
  (require 'color)
  (defun lc/set-csv-separators ()
    (interactive)
    (let* ((n-commas (count-matches "," (point-at-bol) (point-at-eol)))
           (n-semicolons (count-matches ";" (point-at-bol) (point-at-eol))))
      (if ( ; <
           > n-commas n-semicolons)
          (customize-set-variable 'csv-separators '("," " "))
        (customize-set-variable 'csv-separators '(";" " ")))))
  (defun lc/csv-highlight ()
    (interactive)
    (font-lock-mode 1)
    (let* ((separator (string-to-char (car csv-separators)))
           (n (count-matches (string separator) (point-at-bol) (point-at-eol)))
           (colors (loop for i from 0 to 1.0 by (/ 2.0 n)
                         collect (apply #'color-rgb-to-hex
                                        (color-hsl-to-rgb i 0.3 0.5)))))
      (loop for i from 2 to n by 2
            for c in colors
            for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
            do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))
  )

(use-package olivetti
  :general
  (lc/leader-keys
   "t o" '(olivetti-mode :wk "olivetti"))
  :init
  (setq olivetti-body-width 100)
  (setq olivetti-recall-visual-line-mode-entry-state t))
