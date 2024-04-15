;;; lsp.el --- LSP configuration -*- lexical-binding: t; -*-
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
            (pydocstyle (enabled . t)))))))

(provide 'lsp)

;;; lsp.el ends here
