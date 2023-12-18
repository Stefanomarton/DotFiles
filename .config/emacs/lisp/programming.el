;;; programming.el --- Programming languages configuration  -*- lexical-binding: t; -*-

;; (use-package highlight-indent-guides
;;   :hook (prog-mode . highlight-indent-guides-mode)
;;   :custom (highlight-indent-guides-method 'character)
;;   :init
;;   ;; (set-face-background 'highlight-indent-guides-odd-face "#e3e3d3")
;;   ;; (set-face-background 'highlight-indent-guides-even-face "dimgray")
;;   (set-face-foreground 'highlight-indent-guides-character-face "#e3e3d3")
;;   )

;; Python mode setup
(use-package python-mode
  :commands python-mode
  :straight (:type built-in)
  :mode ".py"
  :interpreter "python"
  :init
  (evil-define-key 'normal python-mode-map (kbd "<tab>") 'evil-shift-right-line)
  (evil-define-key 'normal python-mode-map (kbd "<backtab>") 'evil-shift-left-line)
  (evil-define-key 'visual python-mode-map (kbd "<tab>") 'evil-shift-right)
  (evil-define-key 'visual python-mode-map (kbd "<backtab>") 'evil-shift-left)
  )

;; Lua setup

(use-package lua-mode
  :after dashboard)

(use-package lisp-mode
  :straight nil
  :ensure nil
  :config
  (defun auto-byte-recompile ()
    "If the current buffer is in emacs-lisp-mode and there already exists an `.elc'
file corresponding to the current buffer file, then recompile the file."
    (interactive)
    (when (and (eq major-mode 'emacs-lisp-mode)
               (file-exists-p (byte-compile-dest-file buffer-file-name)))
      (byte-compile-file buffer-file-name)))
  (add-hook 'after-save-hook 'auto-byte-recompile)
  (add-to-list 'display-buffer-alist
               '("\\*Compile-Log\\*"
                 (display-buffer-in-direction)
                 (direction . down)
                 (window-width . 0.1)
                 (window-height . 0.1))))

(use-package yasnippet-capf
  :after cape
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(use-package cape
  :init
  ;; Add completion to list
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
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

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package corfu
  :after dashboard
  ;; Optional customizations
  :bind (:map corfu-popupinfo-map
              ("M-d" . corfu-popupinfo-toggle))
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0)
  (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect 'first)      ;; Preselect the prompt
  (corfu-popupinfo-delay (cons nil 1.0)) ;; Autoupdate only after toggling
  :config
  (corfu-popupinfo-mode)
  (add-hook 'org-mode-hook
            (lambda ()
              (setq-local corfu-auto-prefix 2)
              (setq-local completion-at-point-functions
                          '(cape-file
                            yasnippet-capf
                            cape-elisp-block
                            cape-dict
                            cape-dabbrev))))
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq-local corfu-auto-prefix 1)
              (setq-local completion-at-point-functions
                          '(cape-file
                            yasnippet-capf
                            cape-elisp-block
                            cape-elisp-symbol
                            cape-dict
                            cape-dabbrev))))
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  :init
  (global-corfu-mode))

(provide 'programming)

;;; programming.el ends here
