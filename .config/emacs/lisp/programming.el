;;; programming.el --- Programming languages configuration  -*- lexical-binding: t; -*-

;; (use-package highlight-indent-guides
;;   :hook (prog-mode . highlight-indent-guides-mode)
;;   :custom (highlight-indent-guides-method 'character)
;;   :init
;;   ;; (set-face-background 'highlight-indent-guides-odd-face "#e3e3d3")
;;   ;; (set-face-background 'highlight-indent-guides-even-face "dimgray")
;;   (set-face-foreground 'highlight-indent-guides-character-face "#e3e3d3")
;;   )

(use-package treesit
  :straight (:type built-in)
  :config
  (customize-set-variable 'treesit-font-lock-level 4)
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (setq major-mode-remap-alist
        '((css-mode  . css-ts-mode)
          (rust-mode . rust-ts-mode)
          (bash-mode . bash-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (python-mode . python-ts-mode)))
  )

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
  :mode ("\\.lua?\\'" . lua-mode)
  )

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

(use-package yuck-mode
  :mode "\\.yuck\\'"
  )

(use-package rust-mode
  :mode "\\.rs\\'")

(provide 'programming)

;;; programming.el ends here
