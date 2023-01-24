(provide 'core)

(use-package evil
  :straight t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  (setq evil-search-module 'evil-search)
  :config
  (evil-mode 1))

;; Use escape to remove hightlight in normal mode
(evil-define-key 'normal 'global (kbd "<escape>") 'evil-ex-nohighlight)

(use-package evil-collection
  :straight t
  :after evil
  :config
  (evil-collection-init))

(use-package evil-goggles
  :straight t
	:after evil
  :config
  (evil-goggles-mode)
	(setq evil-goggle-duration 0.2)
  (evil-goggles-use-diff-faces))

(use-package undo-fu
  :straight t)

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode 1))

(defun comment_end_of_line ()
  (interactive)
  (call-interactively 'comment-dwim)
  (call-interactively 'evil-append))

(evil-define-key 'normal 'global (kbd "gcA") 'comment_end_of_line)
(evil-define-key 'normal 'global (kbd "gcc") 'comment-line)
(evil-define-key 'visual 'global (kbd "gc") 'comment-line)

(use-package avy
	:straight t
	:config)

(evil-define-key 'normal 'global (kbd "s") 'avy-goto-char-2)
(evil-define-key 'motion 'global (kbd "s") 'avy-goto-char-2)
(evil-define-key 'operator 'global (kbd "s") 'avy-goto-char-2)

(evil-define-key 'normal 'global (kbd "f") 'avy-goto-char-in-line)
(evil-define-key 'motion 'global (kbd "f") 'avy-goto-char-in-line)
(evil-define-key 'operator 'global (kbd "f") 'avy-goto-char-in-line) ;

(use-package which-key
	:straight t
	:init
	(which-key-setup-minibuffer)
	(which-key-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; Enable vertico
(use-package vertico
	:straight t
  :init
  (vertico-mode)
  (setq vertico-count 20)
	(setq vertico-scroll-margin 3)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
	:straight t
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

;; Optionally use the `orderless' completion style.
(use-package orderless
	:straight t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; Vertico postframe
(use-package vertico-posframe
	:straight t
	:init
	:config
	(setq vertico-posframe-min-width 50)
	(setq vertico-posframe-width 70)
	(vertico-posframe-mode 1)
	;; (setq vertico-posframe-parameters
  ;;     '((left-fringe . 8)
  ;;       (right-fringe . 8)))
	)

(evil-define-key 'normal 'global (kbd ":") 'execute-extended-command)

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))
