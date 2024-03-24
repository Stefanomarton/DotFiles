;;; file-management.el -*- lexical-binding: t; -*-

;; Better dired
(use-package dirvish
  :commands (dired-jump dirvish-dwim)
  :config
  (evil-define-key 'normal dirvish-mode-map
    (kbd "j") 'dired-up-directory
    (kbd "k") 'dired-next-line
    (kbd "l") 'dired-previous-line
    (kbd "/") 'dired-find-alternate-file
    (kbd "f") 'dired-narrow-fuzzy
    (kbd "t") 'dired-create-empty-file
    (kbd "<RET>") 'dired-find-alternate-file
    (kbd "q") 'dirvish-quit
    (kbd "e") 'dirvish-layout-toggle
    (kbd "<escape>") 'dired-unmark-all-files
    (kbd "SPC") 'dired-mark
    (kbd "Q") 'dired-create-directory
    )
  (setq dirvish-default-layout '(0.8 0.2 0.5))
  :init
  (dirvish-override-dired-mode)
  )

(use-package dired-narrow
  :after dirvish
  :config
  (defun dired-narrow-ex-ac ()
    ;; Revert buffer and enter the directory after narrowing
    (revert-buffer)
    (dired-find-alternate-file))
  (setq dired-narrow-exit-when-1-left t)
  (setq dired-narrow-exit-action 'dired-narrow-ex-ac)
  )

;; (use-package dired
;;   :commands dired
;;   :straight nil
;;   :ensure nil
;;   :config
;;   (put 'dired-find-alternate-file 'disabled nil)
;;   )

(provide 'file-management)

;;; file-management.el ends here
