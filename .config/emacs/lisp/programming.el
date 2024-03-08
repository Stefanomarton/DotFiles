;;; programming.el --- Programming languages configuration  -*- lexical-binding: t; -*-

;; (use-package highlight-indent-guides
;;   :hook (prog-mode . highlight-indent-guides-mode)
;;   :custom (highlight-indent-guides-method 'character)
;;   :init
;;   ;; (set-face-background 'highlight-indent-guides-odd-face "#e3e3d3")
;;   ;; (set-face-background 'highlight-indent-guides-even-face "dimgray")
;;   (set-face-foreground 'highlight-indent-guides-character-face "#e3e3d3")
;;   )

;; (use-package treesit
;;   :straight (:type built-in)
;;   :config
;;   (customize-set-variable 'treesit-font-lock-level 4)
;;   (setq major-mode-remap-alist
;;         '((css-mode  . css-ts-mode)
;;           (rust-mode . rust-ts-mode)
;;           (python-mode . python-ts-mode)))
;;   )


;; Python mode setup
(use-package python-mode
  :commands python-mode
  :straight (:type built-in)
  :interpreter "python"
  :config
  (evil-define-key 'normal python-mode-map (kbd "<tab>") 'evil-shift-right-line)
  (evil-define-key 'normal python-mode-map (kbd "<backtab>") 'evil-shift-left-line)
  (evil-define-key 'visual python-mode-map (kbd "<tab>") 'evil-shift-right)
  (evil-define-key 'visual python-mode-map (kbd "<backtab>") 'evil-shift-left)
  )

;; Lua setup

(use-package lua-mode
  :after dashboard)

;; (use-package lisp-mode
;;   :straight nil
;;   :ensure nil
;;   :config
;;   (defun auto-byte-recompile ()
;;     "If the current buffer is in emacs-lisp-mode and there already exists an `.elc'
;; file corresponding to the current buffer file, then recompile the file."
;;     (interactive)
;;     (when (and (eq major-mode 'emacs-lisp-mode)
;;                (file-exists-p (byte-compile-dest-file buffer-file-name)))
;;       (byte-compile-file buffer-file-name)))
;;   (add-hook 'after-save-hook 'auto-byte-recompile)
;;   (add-to-list 'display-buffer-alist
;;                '("\\*Compile-Log\\*"
;;                  (display-buffer-in-direction)
;;                  (direction . down)
;;                  (window-width . 0.1)
;;                  (window-height . 0.1))))

;; Highlight kmonad files
(use-package kbd-mode
  :straight (:host github
                   :repo "kmonad/kbd-mode")
  :mode ("\\.kbd\\'" . kbd-mode))


(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))


(use-package indent-bars
  :straight (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :config
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
				                      list list_comprehension
				                      dictionary dictionary_comprehension
				                      parenthesized_expression subscript)))
  :hook
  (python-ts-mode yaml-mode) . (indent-bars-mode)) ; or whichever modes you prefer

(use-package yaml-mode
  :mode "\\.yml\\'"
  )

(provide 'programming)

;;; programming.el ends here
