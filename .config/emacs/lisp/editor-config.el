;;; editor-config.el --- Editor Configurations

;; Editorconfig, auto set indenting
(use-package editorconfig
  :after find-file
  :config
  (editorconfig-mode 1)
  )

;; Code cleanup
(add-hook 'before-save-hook
	      (lambda ()
	        (unless (derived-mode-p 'markdown-mode)
		      (lambda() (delete-trailing-whitespace))
		      )))

(add-hook 'before-save-hook
	      (lambda ()
	        (unless (derived-mode-p 'markdown-mode)
		      'whitespace-cleanup
		      )))

;; Do not remove white spaces in markdown
(unless (derived-mode-p 'markdown-mode)
  (setq nuke-trailing-whitespace-p t))

(add-hook 'before-save-hook
	      (lambda ()
	        (unless (derived-mode-p 'markdown-mode)
		      (lambda() (delete-trailing-whitespace))
		      )))

(add-hook 'before-save-hook
	      (lambda ()
	        (unless (derived-mode-p 'markdown-mode)
		      'whitespace-cleanup
		      )))


;; Autopair parenthesis
(use-package electric
  :straight (:type built-in)
  :init
  (electric-pair-mode +1) ;; automatically insert closing parens
  (setq electric-pair-preserve-balance nil) ;; more annoying than useful
  )

(use-package tab-jump-out)

;; Highlight nested parentheses
(use-package rainbow-delimiters
  :defer 1
  :hook
  (prog-mode . rainbow-delimiters-mode)
  :config
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
		              :foreground "red"
		              :inherit 'error
		              :box t)
  )

;; Highlight colorstring with the right color
(use-package rainbow-mode
  :commands rainbow-mode
  :config
  (add-hook 'prog-mode #'rainbow-mode)
  )

(use-package avy
  :after evil
  :config
  (setq avy-timeout-seconds 0.2)
  (setq avy-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)) ;; Home row only (the default).
  (setq avy-words
	    '("am" "by" "if" "is" "it" "my" "ox" "up"
	      "ace" "act" "add" "age" "ago" "aim" "air"
	      "ale" "all" "and" "ant" "any" "ape" "apt"))

  ;; (evil-define-key 'operator 'global (kbd "l") 'avy-goto-line)

  (evil-define-key 'normal 'global (kbd "<leader> s") 'evil-avy-goto-char-timer)

  (evil-define-key 'normal 'global (kbd "s") 'evil-avy-goto-char-2-below)
  (evil-define-key 'normal 'global (kbd "S") 'evil-avy-goto-char-2-above)

  (evil-define-key 'normal 'global (kbd "F") 'evil-avy-goto-char-in-line-beg)
  (evil-define-key 'visual 'global (kbd "F") 'evil-avy-goto-char-in-line-beg)
  (evil-define-key 'operator 'global (kbd "F") 'evil-avy-goto-char-in-line-beg)

  ;; (evil-define-key 'visual 'global (kbd "F") 'avy-goto-char)

  (evil-define-key 'normal 'global (kbd "f") 'evil-avy-goto-char-in-line-end)
  (evil-define-key 'visual 'global (kbd "f") 'evil-avy-goto-char-in-line-end)
  (evil-define-key 'operator 'global (kbd "f") 'evil-avy-goto-char-in-line-end)
  (evil-define-key 'normal 'global (kbd "C-k") 'pop-global-mark)

  ;; (evil-define-key 'motion 'global (kbd "L") 'avy-copy-line)

  (setq avy-timeout-seconds 0.3)
  (defun avy-action-kill-whole-line (pt)
    ;; Kill action for avy
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)
  (setf (alist-get ?x avy-dispatch-alist) 'avy-action-kill-stay
	    (alist-get ?K avy-dispatch-alist) 'avy-action-kill-whole-line)
  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
	      (bounds-of-thing-at-point 'line)
	    (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)
  (setf (alist-get ?y avy-dispatch-alist) 'avy-action-yank
	    (alist-get ?c avy-dispatch-alist) 'avy-action-copy
	    (alist-get ?C avy-dispatch-alist) 'avy-action-copy-whole-line
	    (alist-get ?Y avy-dispatch-alist) 'avy-action-yank-whole-line)
  )

(use-package expand-region
  :config
  (defun expand-region ()
    "Repeat the `er/expand-region' command."
    (interactive)
    (dotimes (_ 2)
      (call-interactively 'er/expand-region)))
  (setq expand-region-subword-enabled t)
  (evil-define-key 'normal 'global (kbd "gj") 'expand-region)
  )

(use-package aggressive-indent
  :config
  (add-hook 'prog-mode-hook 'aggressive-indent-mode))

(provide 'editor-config)

;;; editor-config.el ends here
