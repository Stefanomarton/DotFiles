;;; keybindings.el --- Useful Tools -*- lexical-binding: t; -*-

(use-package general
  :demand t
  :config

  (defvar general-leader "SPC"
    "Leader key for Evil")

  (defvar general-leader-alt "M-SPC"
    "Leader key for Emacs and Evil Insert states")

  (defvar general-localleader ","
    "Local leader key for major-mode specific commands")

  (defvar general-localleader-alt "M-SPC ,"
    "Local leader key for major-mode specific commands for Emacs and Evil Insert states.")

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

  (general-nmap "c" (general-key-dispatch 'evil-change
                      "c" 'my-evil-change-whole-line))
  (general-vmap "c" 'evil-change)
  :init
  (general-evil-setup t))


(provide 'keybindings)

;;; keybindings.el ends here
