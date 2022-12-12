(provide 'bindings)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(global-set-key (kbd "S-C-c") 'kill-ring-save)
(global-set-key (kbd "S-C-v") 'yank)

;; Which Key
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

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
    "w" '(basic-save-buffer :which-key "Write file")
    "e" '(eval-buffer :which-key "eval buffer")
    "ggl" '(google-this-noconfirm :which-key "Google the selection")))
