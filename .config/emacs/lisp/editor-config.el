;;; editor-config.el -*- lexical-binding: t; -*-

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
  :hook
  (prog-mode . electric-layout-mode)
  (org-mode . electric-layout-mode)
  :config
  (electric-pair-mode +1) ;; automatically insert closing parens
  (setq electric-pair-preserve-balance nil) ;; more annoying than useful
  (setq electric-pair-delete-adjacent-pairs nil) ;; more annoying than useful
  )

(use-package paren
  :defer 2
  :config
  (show-paren-mode)
  (setq show-paren-delay 0.1)
  (setq show-paren-highlight-openparen t)
  (setq show-paren-when-point-inside-paren t)
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
  (setq avy-timeout-seconds 0.5)
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
  :custom
  (expand-region-subword-enabled t)
  (expand-region-smart-cursor t)
  :config
  (add-hook 'LaTeX-mode-hook 'er/set-latex-mode-expansions 90)
  (set-default 'er--show-expansion-message nil)
  (setq expand-region-show-usage-message nil
        expand-region-fast-keys-enabled t
        expand-region-contract-fast-key "-"
        expand-region-reset-fast-key "r")

  (evil-define-key 'normal 'global (kbd "<backspace>") 'er/expand-region)

  (defun er/mark-latex-text-sentence ()
    (unless (texmathp) (er/mark-text-sentence)))
  (defun er/mark-latex-text-paragraph ()
    (unless (texmathp) (er/mark-text-paragraph)))
  (defun er/mark-LaTeX-inside-math ()
    "Mark text inside LaTeX math delimiters. See `er/mark-LaTeX-math'
for details."
    (when (texmathp)
      (let* ((string (car texmathp-why))
             (pos (cdr texmathp-why))
             (reason (assoc string texmathp-tex-commands1))
             (type (cadr reason)))
        (cond
         ((eq type 'sw-toggle) ;; $ and $$
          (goto-char pos)
          (set-mark (1+ (point)))
          (forward-sexp 1)
          (backward-char 1)
          (exchange-point-and-mark))
         ((or (eq type 'sw-on)
              (equal string "Org mode embedded math")) ;; \( and \[
          (re-search-forward texmathp-onoff-regexp)
          (backward-char 2)
          (set-mark (+ pos 2))
          (exchange-point-and-mark))
         (t (error (format "Unknown reason to be in math mode: %s" type)))))))

  (defun er/mark-latex-inside-pairs ()
    (if (texmathp)
        (cl-destructuring-bind (beg . end)
            (my/find-bounds-of-regexps " *[{([|<]"
                                       " *[]})|>]")
          (when-let ((n (length (match-string-no-properties 0))))
            (set-mark (save-excursion
                        (goto-char beg)
                        (forward-char n)
                        (skip-chars-forward er--space-str)
                        (point)))
            (goto-char end)
            (backward-char n)
            (if (looking-back "\\\\right\\\\*\\|\\\\" (- (point) 7))
                (backward-char (length (match-string-no-properties 0)))))
          (skip-chars-backward er--space-str)
          (exchange-point-and-mark))
      (er/mark-inside-pairs)))
  (defun er/mark-latex-outside-pairs ()
    (if (texmathp)
        (cl-destructuring-bind (beg . end)
            (my/find-bounds-of-regexps " *[{([|<]"
                                       " *[]})|>]")
          (set-mark (save-excursion
                      (goto-char beg)
                      ;; (forward-char 1)
                      (if (looking-back "\\\\left\\\\*\\|\\\\" (- (point) 6))
                          (backward-char (length (match-string-no-properties 0))))
                      (skip-chars-forward er--space-str)
                      (point)))
          (goto-char end)
          (skip-chars-backward er--space-str)
          ;; (backward-char 1)
          (exchange-point-and-mark))
      (er/mark-outside-pairs)))
  (defun er/set-latex-mode-expansions ()
    (make-variable-buffer-local 'er/try-expand-list)
    (setq er/try-expand-list
          '(er/mark-word er/mark-symbol er/mark-symbol-with-prefix
                         er/mark-next-accessor  er/mark-inside-quotes er/mark-outside-quotes
                         er/mark-LaTeX-inside-math
                         er/mark-latex-inside-pairs er/mark-latex-outside-pairs
                         er/mark-comment er/mark-url er/mark-email ;er/mark-defun
                         er/mark-latex-text-sentence er/mark-latex-text-paragraph))
    (er/add-latex-mode-expansions)))

(provide 'editor-config)

;;; editor-config.el ends here
