;;; tools.el --- Useful Tools -*- lexical-binding: t; -*-

;; emacs tools
(use-package helpful
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-function] . helpful-function))

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

;; (use-package ellama
;;   :init
;;   (setopt ellama-language "Italian")
;;   (require 'llm-ollama)
;;   (setopt ellama-provider
;; 		  (make-llm-ollama
;; 		   :chat-model "mistral:latest" :embedding-model "mistral:latest")))

(use-package google-this
  :commands google-this
  :defer t
  )

(use-package csv-mode
  :commands csv-mode
  )

(use-package pkg-info
  :defer 2)

(use-package bug-hunter
  :defer 2)

(use-package explain-pause-mode
  :defer 2
  :config
  (explain-pause-mode)
  )


(provide 'tools)

;;; tools.el ends here
