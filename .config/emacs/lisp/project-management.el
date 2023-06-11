;;; project-management.el --- Project management packages

(provide 'project-management)

(use-package projectile
  :defer 1
  :after vertico
  :diminish projectile-mode
  :custom
  (setq projectile-track-known-projects-automatically nil)
  (setq projectile-completion-system 'consult)
  :config
  (define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
  (projectile-mode)
  :init
  (setq projectile-indexing-method 'native)
  (setq projectile-known-projects-file "~/.config/emacs/project.el")
  )

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


;;; project-management.el ends here
