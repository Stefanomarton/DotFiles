;;; lsp.el --- LSP configuration -*- lexical-binding: t; -*-

;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")
;;   :config
;;   (setq lsp-tex-server 'digestif)
;;   (setq lsp-enable-symbol-highlighting nil)
;;   (setq lsp-lens-enable nil)
;;   (setq lsp-completion-provider :none) ;; must have to make yasnippet backend work correctly
;;   (setq lsp-completion-enable t)
;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;   (lsp-headerline-breadcrumb-mode)
;;   :hook
;;   ((lua-mode . lsp)
;;    (lsp-mode . lsp-enable-which-key-integration)
;;    (LaTeX-mode . lsp)
;;    (python-mode . lsp)) ;whichkey-integration
;;   )

;; (use-package lsp-ui
;;   :after lsp
;;   :hook
;;   (lsp-mode . lsp-ui-mode)
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-doc-position 'bottom))

;; (use-package lsp-latex
;;   :after tex
;;   :straight (lsp-latex :type git :host github :repo "ROCKTAKEY/lsp-latex")
;;   :config
;;   (setq lsp-latex-build-forward-search-after nil)
;;   (setq lsp-latex-build-executable "tectonic")
;;   (setq lsp-latex-forward-search-executable "zathura")
;;   (setq lsp-latex-forward-search-args '("--synctex-forward" "%l:1:%f" "%p"))
;;   (setq lsp-latex-build-on-save nil)
;;   (setq lsp-latex-build-args '("-X" "compile" "%f" "--synctex")))

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook
  (python-ts-mode . eglot-ensure)
  (LaTeX-mode . eglot-ensure)
  :config
  (evil-define-key 'normal python-ts-mode-map (kbd "<tab>") 'evil-shift-right-line)
  (evil-define-key 'normal python-ts-mode-map (kbd "<backtab>") 'evil-shift-left-line)
  (evil-define-key 'visual python-ts-mode-map (kbd "<tab>") 'evil-shift-right)
  (evil-define-key 'visual python-ts-mode-map (kbd "<backtab>") 'evil-shift-left)
  (setq eglot-workspace-configuration
        '((pylsp
           (plugins
            (jedi_completion (fuzzy . t))
            (pydocstyle (enabled . t))))))
  )

(provide 'lsp)

;;; lsp.el ends here
