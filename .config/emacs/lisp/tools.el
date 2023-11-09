;;; tools.el --- Useful Tools -*- lexical-binding: t; -*-

;; emacs tools
(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

;; Startup time evaluation
(use-package esup
  :config
  (setq esup-depth 0)
  :commands esup)

(use-package vterm
  :commands vterm
  :config
  ;; (set-fontset-font t 'unicode (font-spec :family "JetBrainsMono Nerd Font"))
  :custom
  (setq term-toggle-no-confirm-exit t)
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"
	    vterm-internal-use-ligatures t
	    vterm-max-scrollback 10000
	    vterm-shell "zsh"
	    ))

(use-package google-this
  :defer t
  )

(use-package csv-mode
  :commands csv-mode
  )

(provide 'tools)

;;; tools.el ends here
