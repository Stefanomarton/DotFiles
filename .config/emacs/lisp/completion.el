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
  :config

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

(use-package posframe
  :after vertico)

(use-package vertico-posframe
  :after posframe
  ;; :init
  ;; (custom-set-faces
  ;;  '(vertico-posframe ((t (:background "#090a0c"))))
  ;;  '(vertico-posframe-border ((t (:background "#B9788C")))))
  ;; :custom-face
  ;; (vertico-posframe ((t (:background "#090a0c"))))
  ;; (vertico-posframe-border ((t (:background "#B9788C"))))
  :custom
  (vertico-posframe-width 150)
  (vertico-posframe-border-width 2)
  (vertico-multiform-commands
   '((execute-extended-command posframe)
     (consult-outline buffer ,(lambda (_) (text-scale-set -1)))
     (:not posframe))))

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
  :init
  (marginalia-mode))

;; musthave
(use-package consult
  :init
  (setq consult-preview-allowed-hooks '(global-font-lock-mode-check-buffers save-place-find-file-hook display-line-numbers-mode))
  :general
  (:states 'normal
           "<leader>cp" '(consult-yank-from-kill-ring :no-autoload t))
  (:states 'insert
           "C-c p" '(consult-yank-from-kill-ring :no-autoload t))
  :defer 1)

(use-package consult-projectile
  :commands (consult-projectile))

(use-package consult-dir
  :after consult)

(use-package consult-todo
  :straight (:host github :repo "liuyinz/consult-todo")
  :after consult
  :config
  (defconst consult-todo--narrow
    '((?t . "TODO")
      (?f . "FIXME")
      (?b . "BUG")
      (?h . "ASK"))
    "Default mapping of narrow and keywords.")
  )

(use-package cape
  :init
  ;; Add completion to list
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )

(use-package corfu
  :bind
  (:map corfu-map ("C-c" . corfu-insert-separator))
  :after dashboard
  ;; Optional customizations
  :bind (:map corfu-popupinfo-map
              ("M-d" . corfu-popupinfo-toggle))
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0)
  (corfu-bar-width 0)
  (corfu-right-margin-width 1)
  (corfu-left-margin-width 1)
  (corfu-min-width 10)
  (corfu-max-width 80)
  (corfu-separator nil)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'first)      ;; Preselect the prompt
  (corfu-popupinfo-delay (cons nil 1.0)) ;; Autoupdate only after toggling
  :config

  (corfu-popupinfo-mode)
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
                           (cape-capf-prefix-length #'yasnippet-capf 1)
                           ;; (cape-capf-prefix-length #'cape-dabbrev 3)
                           ))))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local corfu-auto-prefix 1)
              (setq-local completion-at-point-functions
                          '(cape-file
                            yasnippet-capf
                            cape-keyword
                            cape-elisp-symbol
                            cape-dict
                            cape-dabbrev))))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input)
                (eq (current-local-map) read-passwd-map))
      (setq-local corfu-echo-delay nil ;; Disable automatic echo and popup
                  corfu-popupinfo-delay nil
                  corfu-min-width 10
                  corfu-max-width 80
                  )
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  (use-package pcomplete
    :defer
    :config
    ;; Silence the pcomplete capf, no errors or messages!
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)
    ;; (advice-remove 'pcomplete-completions-at-point #'cape-wrap-silent)

    ;; Ensure that pcomplete does not write to the buffer
    ;; and behaves as a pure `completion-at-point-function'.
    (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-purify)
    ;; (advice-remove 'pcomplete-completions-at-point #'cape-wrap-purify)
    )
  :init
  (global-corfu-mode))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'completion)

;;; completion.el ends here
