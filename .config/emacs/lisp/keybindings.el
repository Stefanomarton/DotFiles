;;; keybindings.el --- Useful Tools -*- lexical-binding: t; -*-

(use-package general
  :preface
  ;; find-file keybindings
  (defun my/find-file (directory)
    (interactive)
    (let ((default-directory directory))
      (ido-find-file)))

  ;; Fix change whole line
  (defun my-evil-change-whole-line ()
    (interactive)
    (beginning-of-line)
    (evil-change-line (point) (line-end-position)))

  (defun my-evil-change-visual-selection ()
    "Replace the region with an empty line and enter insert mode."
    (interactive)
    (let ((start (region-beginning))
          (end (region-end)))
      (delete-region start end)
      (goto-char start)
      (open-line 1)
      (evil-insert 1)))

  ;; Fix visual end-of-line and custom function
  (defun my-visual-line ()
    (interactive)
    (evil-normal-state)
    (evil-first-non-blank)
    (evil-visual-state)
    (evil-end-of-line)
    (evil-backward-char)
    )

  :demand t
  :config
  ;; fix unbindings
  (general-auto-unbind-keys)

  ;; general leader
  (defvar general-leader "SPC"
    "Leader key for Evil")




  (general-create-definer file-leader
    :prefix "C-f")

  (file-leader
    :states '(normal insert)
    "C-f" 'find-file
    "h" '(lambda () (interactive) (my/find-file "~/"))
    "d" '(lambda () (interactive) (my/find-file "~/GoogleDrive/"))
    "c" '(lambda () (interactive) (my/find-file "~/.config"))
    "u" '(lambda () (interactive) (my/find-file "~/GoogleDrive/University/"))
    "o" '(lambda () (interactive) (my/find-file "~/GoogleDrive/org"))
    "p" 'consult-projectile-find-file
    "w" 'find-file-other-window)



  ;; buffer keybinding
  (general-create-definer buffer-leader
    :prefix "C-b")

  (buffer-leader
    :states '(normal insert)
    "C-b" 'consult-buffer)



  ;; project keybinding
  ;; buffer keybinding
  (general-create-definer project-leader
    :prefix "C-p")

  (project-leader
    :states '(insert normal)
    "C-p" 'consult-projectile
    "s" 'consult-projectile-switch-project
    )



  (general-nmap "c" (general-key-dispatch 'evil-change
                      "c" 'my-evil-change-whole-line))
  (general-vmap "c" 'evil-change)

  (general-vmap "v" 'my-visual-line)


  :init
  (general-evil-setup t))


(provide 'keybindings)

;;; keybindings.el ends here
