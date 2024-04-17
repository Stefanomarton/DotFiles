;;; completion.el --- Useful Tools -*- lexical-binding: t; -*-

;; Enable vertico for the best vertical completion experience
(use-package vertico
  :defer 1
  :bind
  (:map vertico-map
	    ("C-e" . embark-minimal-act)
	    ("C-k" . vertico-next)
	    ("C-l" . vertico-previous)
	    ("<escape>" . keyboard-escape-quit))
  :config

  (add-to-list 'vertico-multiform-categories
               '(jinx grid (vertico-grid-annotate . 20)))

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
	    '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  ;; Indicated in the documentation
  (setq fast-but-imprecise-scrolling t
	    jit-lock-defer-time 0)

  ;; Ignore case when completing
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  ;; Different scroll margin
  (setq vertico-scroll-margin 2)

  ;; Show more candidates
  (setq vertico-count 10)
  (setq vertico-resize nil)
  (setq vertico-cycle t)

  :init
  (vertico-multiform-mode)
  (vertico-mode))

;; orderless completion method
(use-package orderless
  :config
  (setq orderless-affix-dispatch-alist nil)
  (setq completion-styles '(orderless basic)
	    completion-category-defaults nil
	    completion-category-overrides '((file (styles partial-completion)))))

;; i like some help while searching
(use-package marginalia
  :defer .1
  :after vertico
  :config
  (marginalia-mode))

;; musthave
(use-package consult
  :defer 1
  :init
  (setq consult-preview-allowed-hooks '(global-font-lock-mode-check-buffers save-place-find-file-hook display-line-numbers-mode))
  :general
  (:states 'normal
           "<leader>cp" '(consult-yank-from-kill-ring :no-autoload t))
  (:states 'insert
           "C-c p" '(consult-yank-from-kill-ring :no-autoload t)
           "C-c l" '(consult-line :no-autoload t)
           )
  )

(use-package consult-projectile
  :commands (consult-projectile))

(use-package consult-dir
  :after consult)

(use-package consult-todo
  :straight (:host github :repo "liuyinz/consult-todo")
  :after consult
  :config
  (setq consult-todo-narrow
        '((?t . "TODO")
          (?f . "FIX")
          (?b . "BUG")
          (?h . "ASK"))))

(use-package cape
  :after corfu
  :config
  (setq cape-dict-file '("/usr/share/dict/italian" "/usr/share/dict/british-english")))

(use-package corfu
  :bind
  (:map corfu-popupinfo-map
        ("M-d" . corfu-popupinfo-toggle))
  :config
  (setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (setq corfu-auto t)                 ;; Enable auto completion
  (setq corfu-auto-prefix 1)
  (setq corfu-auto-delay 0)
  (setq corfu-bar-width 0)
  (setq corfu-right-margin-width 1)
  (setq corfu-left-margin-width 1)
  (setq corfu-min-width 10)
  (setq corfu-max-width 80)
  (setq corfu-separator nil)          ;; Orderless field separator
  (setq corfu-quit-at-boundary 'seperator)   ;; Never quit at completion boundary
  (setq corfu-quit-no-match t)      ;; Never quit, even if there is no match
  (setq corfu-preview-current nil)    ;; Disable current candidate preview
  (setq corfu-preselect 'first)      ;; Preselect the prompt
  (setq corfu-popupinfo-delay (cons nil 1.0)) ;; Autoupdate only after toggling

  ;; Enable popuinfo
  (corfu-popupinfo-mode)

  ;; Corfu for org mode setup
  (add-hook 'org-mode-hook
            (lambda ()
              ;; (setq-local corfu-auto-prefix 1)
              (setq-local completion-at-point-functions
                          '(
                            cape-file
                            yasnippet-capf
                            ;; cape-elisp-block
                            ;; cape-dict
                            ;; citar-capf
                            ;; cape-dabbrev
                            ))
              (setq-local completion-at-point-functions
                          (list
                           ;; (cape-capf-prefix-length #'cape-dict 3)
                           (cape-capf-prefix-length #'cape-file 1)
                           (cape-capf-prefix-length #'yasnippet-capf 2)
                           (cape-capf-prefix-length #'cape-dabbrev 5)
                           ))))

  ;; Eglot and corfu setup

  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'yasnippet-capf
                       #'cape-file))))

  (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)


  ;; Setup for emacs lisp-mode
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local corfu-auto-prefix 1)
              (setq-local completion-at-point-functions
                          '(cape-file
                            yasnippet-capf
                            cape-keyword
                            cape-elisp-symbol
                            cape-dabbrev))))

  ;; Enable corfu in the minibuffer
  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil
                  corfu-auto-prefix 3
                  corfu-preselect 'valid
                  corfu-min-width 10
                  corfu-max-width 80
                  )
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'completion)

;;; completion.el ends here
