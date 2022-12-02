(provide 'orgconfig)


(setq auto-mode-alist (cons '("\\.org$" . org-mode) auto-mode-alist)) ; Enable Latex-Mode when entry in a .tex file
(setq auto-mode-alist (cons '("\\.org$" . org-cdlatex-mode) auto-mode-alist)) ; Enable Latex-Mode when entry in a .tex file
(add-hook 'org-mode-hook 'org-toggle-pretty-entities)
