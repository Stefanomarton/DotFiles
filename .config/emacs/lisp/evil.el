;;; evil.el --- Evil configuration

(use-package evil
  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-want-keybinding nil)
  (setq evil-undo-system 'undo-fu)
  (setq evil-want-integration t)
  (setq evil-move-beyond-eol nil)
  (setq evil-kill-on-visual-paste nil)
  (setq evil-want-empty-ex-last-command t)

  :custom
  (completion-in-region-function 'consult-completion-in-region)
  (evil-operator-state-cursor nil)
  (evil-jumps-cross-buffers t)
  (evil-want-C-u-scroll nil)
  (evil-want-C-d-scroll nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-search-module 'evil-search)
  (evil-esc-delay 0.0001)
  (evil-want-Y-yank-to-eol nil)


  :config

  (mapc #'evil-declare-abort-repeat
        '(balance-windows
          eval-expression
          execute-extended-command
          exit-minibuffer
          compile
          delete-window
          delete-other-windows
          find-file-at-point
          ffap-other-window
          recompile
          redo
          save-buffer
          split-window
          split-window-horizontally
          split-window-vertically
          undo
          undo-tree-redo
          undo-tree-undo))

  (setq evil-want-fine-undo t)
  (setq evil-echo-state nil)

  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))
  (evil-set-leader 'insert (kbd "M-SPC"))

  ;; Window focus
  (evil-define-key 'normal 'evil-normal-state-map
    (kbd "C-w j") 'evil-window-left
    (kbd "C-w /") 'evil-window-right
    (kbd "C-w k") 'evil-window-down
    (kbd "C-w l") 'evil-window-up)

  (evil-define-key 'normal prog-mode-map (kbd "<return>") 'evil-avy-goto-char-timer)
  (evil-define-key 'normal org-mode-map (kbd "<return>") 'evil-avy-goto-char-timer)
  (evil-define-key 'normal LaTeX-mode-map (kbd "<return>") 'evil-avy-goto-char-timer)
  (evil-define-key 'normal markdown-mode-map (kbd "<return>") 'evil-avy-goto-char-timer)

  (evil-define-key 'normal 'evil-normal-state-map (kbd "C-z") 'evil-search-forward)
  (evil-define-key 'normal 'evil-normal-state-map (kbd "C-S-z") 'evil-search-backward)

  (evil-define-key 'normal 'evil-insert-state-map (kbd "C-d") 'kill-word)
  (evil-define-key 'normal 'evil-normal-state-map (kbd "M-d") 'downcase-dwim)

  (evil-define-key '(insert normal) 'evil-normal-state-map (kbd "C-s C-s") (lambda () (interactive)
	                                                                         (yas-expand-snippet "\\\\($1\\\\) $0")))

  (evil-define-key '(normal visual replace operator motion emacs) 'global
    (kbd "j") 'evil-backward-char
    (kbd "k") 'evil-next-visual-line
    (kbd "l") 'evil-previous-visual-line
    (kbd "/") 'evil-forward-char
    )

  (evil-define-key 'normal 'global (kbd "C-m") 'point-to-register)
  (evil-define-key 'normal 'global (kbd "gm") 'jump-to-register)

  (evil-define-key 'insert 'global (kbd "C-y") 'evil-paste-after)

  (defun smart-for-files ()
    (interactive)
    (if (projectile-project-p)
        (consult-projectile-find-file)
      (call-interactively #'find-file)))

  (defun smart-for-buffer ()
    (interactive)
    (if (projectile-project-p)
        (consult-projectile-switch-to-buffer)
      (call-interactively #'consult-buffer)))

  (defun smart-for-terminal-otherw ()
    (interactive)
    (if (projectile-project-p)
        (projectile-run-vterm-other-window)
      (call-interactively #'vterm-other-window)))

  (defun smart-for-terminal ()
    (interactive)
    (if (projectile-project-p)
        (projectile-run-vterm)
      (call-interactively #'vterm)))

  (defun split-and-follow-horizontally ()
    (interactive)
    (split-window-below)
    (balance-windows)
    (other-window 1))

  (defun split-and-follow-vertically ()
    (interactive)
    (split-window-right)
    (balance-windows)
    (other-window 1))

  (defun my-kill-this-buffer ()
    (interactive)
    (catch 'quit
      (save-window-excursion
        (let (done)
	      (when (and buffer-file-name (buffer-modified-p))
	        (while (not done)
	          (let ((response (read-char-choice
			                   (format "Save file %s? (y, n, d, q) " (buffer-file-name))
			                   '(?y ?n ?d ?q))))
	            (setq done (cond
			                ((eq response ?q) (throw 'quit nil))
			                ((eq response ?y) (save-buffer) t)
			                ((eq response ?n) (set-buffer-modified-p nil) t)
			                ((eq response ?d) (diff-buffer-with-file) nil))))))
	      (kill-buffer (current-buffer))))))

  ;; Recompile and reload yasnippet files.
  (defun yas-reload ()
    (interactive)
    (yas-recompile-all)
    (yas-reload-all)
    )


  (evil-define-key 'insert 'global (kbd "C-y") 'evil-paste-after)
  (evil-define-key 'normal 'evil-normal-state-map (kbd "C-u") 'evil-scroll-up)

  (evil-define-key 'normal 'global
    (kbd ":") 'evil-ex
    (kbd ";") 'execute-extended-command

    (kbd "<leader>ff") 'find-file
    (kbd "<leader>fd") 'my/find-file-google
    (kbd "<leader>fh") 'my/find-file-home
    (kbd "<leader>fc") 'my/find-file-config
    (kbd "<leader>fw") 'find-file-other-window
    (kbd "<leader>fr") 'consult-recent-file
    (kbd "<leader>fg") 'consult-grep

    (kbd "<leader>r") 'consult-register
    (kbd "<leader>dj") 'dirvish
    (kbd "<leader>dD") 'dired
    (kbd "<leader>dd") 'consult-dir
    (kbd "<leader>bb") 'smart-for-buffer
    (kbd "<leader>w") 'save-buffer
    (kbd "<leader> q b") 'kill-buffer

    (kbd "Q") 'my-kill-this-buffer
    (kbd "<leader>gt") 'google-this
    (kbd "<leader>gh") 'dashboard-open
    (kbd "<leader>pp") 'consult-projectile
    (kbd "<leader>ps") 'consult-projectile-switch-project

    (kbd "<leader>cc") 'calc
    (kbd "<leader>co") 'consult-outline
    (kbd "<leader>cf") 'consult-focus-lines
    (kbd "<leader>cl") 'consult-line
    (kbd "<leader>cm") 'consult-global-mark
    (kbd "<leader>ci") 'consult-imenu

    (kbd "<leader>aw") 'avy-goto-word-or-subword-1
    (kbd "<leader>al") 'avy-goto-line
    (kbd "<leader>arm") 'avy-move-region
    (kbd "<leader>arc") 'avy-copy-region
    (kbd "<leader>ark") 'avy-kill-region

    (kbd "<leader>qc") 'quick-calc
    (kbd "<leader>t") 'smart-for-terminal-otherw
    (kbd "<leader>T") 'smart-for-terminal
    (kbd "<leader>gg") 'magit-status-with-removed-dotfiles-args
    (kbd "<leader>gd") 'dotfiles-magit-status
    (kbd "<leader>h") 'consult-todo
    (kbd "<leader>H") 'consult-todo-dir
    (kbd "<leader>y") 'yas-reload
    )

  (evil-define-key 'normal emacs-lisp-mode-map
    (kbd "<leader>ee") 'eval-buffer
    (kbd "<leader>es") 'eval-expression
    (kbd "<leader>er") 'eval-region
    (kbd "<leader>ef") 'eval-defun
    )

  (evil-define-key '(normal visual) 'global
    (kbd "<leader> nn") 'narrow-to-region
    (kbd "<leader> nw") 'widen
    (kbd "<leader> np") 'narrow-to-page
    (kbd "<leader> nf") 'narrow-to-defun
    (kbd "<leader> ng") 'goto-line-relative
    (kbd "<leader> df") 'evil-goto-definition
    (kbd "gu") 'evil-next-close-paren
    (kbd "gd") 'evil-previous-open-paren
    )

  (evil-define-key 'insert 'global (kbd "C-<backspace>") 'evil-delete-backward-word)
  (evil-define-key 'visual 'global (kbd "<leader>gg") 'google-this-noconfirm)
  (evil-define-key 'normal 'prog-mode-map (kbd "<leader>m") 'rainbow-mode)

  (evil-mode 1))

;; ;; Undo for evil
(use-package undo-fu
  :after evil)

;; ;; Persisten undo
(use-package undo-fu-session
  :after undo-fu
  :config
  (setq undo-fu-session-linear t)
  (undo-fu-session-global-mode))

;; Evil integrations
(use-package evil-collection
  :defer .5
  :config
  (evil-collection-init))

(use-package evil-terminal-cursor-changer
  :after evil
  :config
  (unless (display-graphic-p)
    (evil-terminal-cursor-changer-activate) ; or (etcc-on)
    ))

(use-package evil-surround
  ;;:straight (:host github :repo "emacs-evil/evil-surround/")
  :after evil
  :commands (evil-surround-edit evil-surround-change evil-surround-delete)
  :init
  (evil-surround-mode)
  :config
  (evil-define-key '(normal visual) 'global (kbd "q") 'evil-surround-edit)
  (evil-define-key '(normal visual) 'global (kbd ", c") 'evil-surround-change)
  (evil-define-key '(normal visual) 'global (kbd ", d") 'evil-surround-delete)
  (setq-default evil-surround-pairs-alist
                '((?j . ("(" . ")"))
                  (?k . ("[" . "]"))
                  (?l . ("{" . "}"))
                  (?\( . ("(" . ")"))
                  (?\[ . ("[" . "]"))
                  (?\{ . ("{" . "}"))
                  (?> . ("<" . ">"))
                  (?t . evil-surround-read-tag)
                  ;; (?< . evil-surround-read-tag)
                  (?\C-f . evil-surround-prefix-function)
                  (?f . evil-surround-function)))
  (add-hook 'org-mode-hook (lambda ()
    		                 (push '(?h . ("\$" . "\$")) evil-surround-pairs-alist)
    		                 (push '(?H . ("\$$" . "\$$")) evil-surround-pairs-alist)
    		                 (push '(?f . ("\\frac{" . "}{}")) evil-surround-pairs-alist)
    		                 (push '(?w . ("\\(\\ce{" . "}\\)")) evil-surround-pairs-alist)
    		                 (push '(?v . ("_{" . "}")) evil-surround-pairs-alist)
    		                 (push '(?, . ("^{" . "}")) evil-surround-pairs-alist)))
  ;; (add-hook 'LaTeX-mode-hook (lambda ()
  ;;   		                   (push '(?p . ("\(" . "\)")) evil-surround-pairs-alist)
  ;;   		                   (push '(?s . ("\[" . "\]")) evil-surround-pairs-alist)
  ;;   		                   (push '(?c . ("{" . "}")) evil-surround-pairs-alist)))
  (add-hook 'markdown-mode-hook (lambda ()
                                  (push '(?* . ("**" . "**")) evil-surround-pairs-alist))))

;; (use-package embrace
;;   :after evil)

;; (use-package evil-embrace
;;   :after embrace
;;   :config
;;   (evil-embrace-enable-evil-surround-integration)
;;   (add-hook 'org-mode-hook
;;             (lambda ()
;;               (embrace-add-pair ?c "{" "}")
;;               (embrace-add-pair ?p "\(" "\)")
;;               (embrace-add-pair ?s "\[" "\]"))))

(use-package evil-commentary
  ;; Better Comment Action
  :after evil
  :config
  ;; Comment at the end of the line
  (defun comment-end-of-line ()
    (interactive)
    (indent-for-comment)
    (evil-insert 1)
    )
  (evil-define-key 'normal 'global (kbd "gcA") 'comment-end-of-line)

  ;; Comment box
  (evil-define-key 'visual 'global (kbd "gb") 'comment-box)

  ;; Comment regione
  (evil-define-key 'visual 'global (kbd "gc") 'evil-commentary)

  ;; Comment current line
  (evil-define-key 'normal 'global (kbd "gcc") 'evil-commentary-line)

  ;; Open a command above
  (defun comment-line-above ()
    (interactive)
    (evil-open-above 1)
    (insert (concat comment-start))
    )
  (evil-define-key 'normal 'global (kbd "gco") 'comment-line-above)

  ;; Open a command below
  (defun comment-line-above ()
    (interactive)
    (evil-open-below 1)
    (insert (concat comment-start))
    )
  (evil-define-key 'normal 'global (kbd "gcO") 'comment-line-above)
  )

(use-package evil-tex
  :defer t
  :hook
  (LaTeX-mode . evil-tex-mode)
  (org-mode . evil-tex-mode)
  ;; :config
  ;; (setq evil-tex-toggle-override-m t)
  )

(use-package more-evil-avy
  :after avy
  :straight (:host github :repo "Stefanomarton/more-evil-avy"))

(use-package evil-owl
  :after evil
  :config
  (setq evil-owl-idle-delay 0.1)
  (setq evil-owl-display-method 'window
        evil-owl-extra-posframe-args '(:width 50 :height 20)
        evil-owl-max-string-length 50)
  (evil-owl-mode)
  )

;; go to last edit with g;
(use-package goto-chg
  :after bind-key
  :bind (:map evil-normal-state-map
              ("<escape>" . evil-goto-last-change)
              ("S-<escape>" . evil-goto-last-change-reverse)))

;; better object with h
(use-package evil-textobj-syntax
  :after evil
  )

;; matching tag with %
(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1)
  )

(use-package evil-lion
  ;; Align string and caracther based of a char of choice using gl (align left) and GL (align right)
  :after evil
  :config
  (evil-lion-mode))

(provide 'evil)

;;; evil.el ends here
