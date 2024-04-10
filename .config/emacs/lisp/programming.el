;;; programming.el --- Programming languages configuration  -*- lexical-binding: t; -*-

(use-package treesit
  :straight (:type built-in)
  :preface
  (defun mp-setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (dolist (grammar
             '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
               (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
               (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
               (css . ("https://github.com/tree-sitter/tree-sitter-css"))
               (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
               (go . ("https://github.com/tree-sitter/tree-sitter-go"))
               (html . ("https://github.com/tree-sitter/tree-sitter-html"))
               (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "master" "src"))
               (json . ("https://github.com/tree-sitter/tree-sitter-json"))
               (make . ("https://github.com/alemuller/tree-sitter-make"))
               (markdown . ("https://github.com/ikatyang/tree-sitter-markdown"))
               (python . ("https://github.com/tree-sitter/tree-sitter-python"))
               (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
               (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))
               (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
               (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))))
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

  ;;Tree-sitter enabled major modes are
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (js2-mode . js-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))

  :config
  (mp-setup-install-grammars))

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
  (indent-bars-treesit-scope '((python function_definition class_definition for_statement
	                                   if_statement with_statement while_statement)))
  ;; (indent-bars-treesit-wrap '((python argument_list parameters ; for python, as an example
  ;;   			                      list list_comprehension
  ;;   			                      dictionary dictionary_comprehension
  ;;   			                      parenthesized_expression subscript)))
  :hook
  (python-ts-mode yaml-ts-mode) . (indent-bars-mode)) ; or whichever modes you prefer

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
