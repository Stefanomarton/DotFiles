;;; tools.el --- Useful Tools

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))

(use-package google-this
  :defer t
  )

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

(use-package gptel
  :commands gptel)

(use-package csv-mode
  :commands csv-mode
  )

;; (use-package langtool
;;   :commands langtoolcheck
;;   :config
;;   (setq langtool-java-classpath
;; 	    "/usr/share/languagetool:/usr/share/java/languagetool/*"))

;; (use-package flycheck-languagetool
;;   :after flycheck
;;   :requires flyckeck langtool
;;   :init
;;   (setq flycheck-languagetool-server-jar "~/Downloads/LanguageTool-6.1/languagetool-server.jar"))

(provide 'tools)

;;; tools.el ends here
