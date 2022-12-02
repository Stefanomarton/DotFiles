(provide 'latexconfig)

(setq auto-mode-alist (cons '("\\.tex$" . latex-mode) auto-mode-alist)) ; Enable Latex-Mode when entry in a .tex file
(setq auto-mode-alist (cons '("\\.tex$" . cdlatex-mode) auto-mode-alist)) ; Enable cdlatex-Mode when entry in a .tex file
;; CDLatex integration with YaSnippet: Allow cdlatex tab to work inside Yas fields
(use-package cdlatex
  :hook ((cdlatex-tab . yas-expand)
         (cdlatex-tab . cdlatex-in-yas-field))
  :config
  (use-package yasnippet
    :bind (:map yas-keymap
 								("<tab>" . yas-next-field-or-cdlatex)
 								("TAB" . yas-next-field-or-cdlatex)
 								("<backtab>" . yas-prev-field))
    :config
    (defun cdlatex-in-yas-field ()
      ;; Check if we're at the end of the Yas field
      (when-let* ((_ (overlayp yas--active-field-overlay))
                  (end (overlay-end yas--active-field-overlay)))
        (if (>= (point) end)
            ;; Call yas-next-field if cdlatex can't expand here
            (let ((s (thing-at-point 'sexp)))
              (unless (and s (assoc (substring-no-properties s)
                                    cdlatex-command-alist-comb))
                (yas-next-field-or-maybe-expand)
                t))
          ;; otherwise expand and jump to the correct location
          (let (cdlatex-tab-hook minp)
            (setq minp
                  (min (save-excursion (cdlatex-tab)
                                       (point))
                       (overlay-end yas--active-field-overlay)))
            (goto-char minp) t))))

    (defun yas-next-field-or-cdlatex nil
      (interactive)
      "Jump to the next Yas field correctly with cdlatex active."
      (if
          (or (bound-and-true-p cdlatex-mode)
              (bound-and-true-p org-cdlatex-mode))
          (cdlatex-tab)
        (yas-next-field-or-maybe-expand)))))

;; Autosnippets configuration with auto-activating-snippets
(use-package aas
  :hook (LaTeX-mode . aas-activate-for-major-mode)
  :hook (org-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'text-mode
										;; expand unconditionally
										";o-" "ō"
										";i-" "ī"
										";a-" "ā"
										";u-" "ū"
										";e-" "ē")
  (aas-set-snippets 'latex-mode
										;; set condition!
										:cond #'texmathp ; expand only while in math
										"supp" "\\supp"
										"On" "O(n)"
										"O1" "O(1)"
										"Olog" "O(\\log n)"
										"Olon" "O(n \\log n)"
										;; bind to functions
										"Span" (lambda () (interactive)
														 (yas-expand-snippet "\\Span($1)$0")))
  ;; disable snippets by redefining them with a nil expansion
  (aas-set-snippets 'latex-mode
										"supp" nil))

(use-package laas
  :hook (LaTeX-mode . laas-mode)
  :hook (org-mode . laas-mode)
  :config ; do whatever here
  (aas-set-snippets 'laas-mode
                    ;; set condition!
                    :cond #'texmathp ; expand only while in math
                    "supp" "\\supp"
                    "On" "O(n)"
                    "O1" "O(1)"
                    "Olog" "O(\\log n)"
                    "Olon" "O(n \\log n)"
                    ;; bind to functions!
                    "sum" (lambda () (interactive)
                            (yas-expand-snippet "\\sum_{$1}^{$2} $0"))
                    "Span" (lambda () (interactive)
                             (yas-expand-snippet "\\Span($1)$0"))
                    ;; add accent snippets
                    :cond #'laas-object-on-left-condition
                    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

;; Yasnippet settings for latex
(use-package yasnippet
  :ensure t
  :hook ((LaTeX-mode . yas-minor-mode)
         (post-self-insert . my/yas-try-expanding-auto-snippets))
  :hook ((markdown-mode . yas-minor-mode)
         (post-self-insert . my/yas-try-expanding-auto-snippets))
  :hook ((org-mode . yas-minor-mode)
         (post-self-insert . my/yas-try-expanding-auto-snippets))
  :config
  (use-package warnings
    :config
    (cl-pushnew '(yasnippet backquote-change)
                warning-suppress-types
                :test 'equal))

  (setq yas-triggers-in-field t)

	;; Suppress warnings
	(with-eval-after-load 'warnings
		(cl-pushnew '(yasnippet backquote-change) warning-suppress-types
								:test 'equal))

  ;; Function that tries to autoexpand YaSnippets
  ;; The double quoting is NOT a typo!
  (defun my/yas-try-expanding-auto-snippets ()
    (when (and (boundp 'yas-minor-mode) yas-minor-mode)
      (let ((yas-buffer-local-condition ''(require-snippet-condition . auto)))
        (yas-expand)))))

;; AucTeX settings - almost no changes
(use-package latex
  :ensure auctex
  :hook ((LaTeX-mode . prettify-symbols-mode))
  :bind (:map LaTeX-mode-map
              ("C-S-e" . latex-math-from-calc))
  :config
  ;; Format math as a Latex string with Calc
  (defun latex-math-from-calc ()
    "Evaluate `calc' on the contents of line at point."
    (interactive)
    (cond ((region-active-p)
           (let* ((beg (region-beginning))
                  (end (region-end))
                  (string (buffer-substring-no-properties beg end)))
             (kill-region beg end)
             (insert (calc-eval `(,string calc-language latex
                                          calc-prefer-frac t
                                          calc-angle-mode rad)))))
          (t (let ((l (thing-at-point 'line)))
               (end-of-line 1) (kill-line 0)
               (insert (calc-eval `(,l
                                    calc-language latex
                                    calc-prefer-frac t
                                    calc-angle-mode rad))))))))


(use-package reftex)


																				; Set custom prefix for math
(defcustom cdlatex-math-symbol-prefix ?:
  "Prefix key for `cdlatex-math-symbol'.
This may be a character, a string readable with `read-kbd-macro', or a
Lisp vector."
  :group 'cdlatex-math-support
  :type '(choice
          (character)
          (string :value "" :tag "kbd readable string")
          (sexp :value [] :tag "a lisp vector")))

;;(setq cdlatex-math-modify-prefix ?:)


;; (use-package lazytab)
;; ;; Array/tabular input with org-tables and cdlatex
;; (use-package org-table
;;   :after cdlatex
;;   :bind (:map orgtbl-mode-map
;;               ("<tab>" . lazytab-org-table-next-field-maybe)
;;               ("TAB" . lazytab-org-table-next-field-maybe))
;;   :init
;;   (add-hook 'cdlatex-tab-hook 'lazytab-cdlatex-or-orgtbl-next-field 90)
;;   ;; Tabular environments using cdlatex
;;   (add-to-list 'cdlatex-command-alist '("smat" "Insert smallmatrix env"
;;                                        "\\left( \\begin{smallmatrix} ? \\end{smallmatrix} \\right)"
;;                                        lazytab-position-cursor-and-edit
;;                                        nil nil t))
;;   (add-to-list 'cdlatex-command-alist '("bmat" "Insert bmatrix env"
;;                                        "\\begin{bmatrix} ? \\end{bmatrix}"
;;                                        lazytab-position-cursor-and-edit
;;                                        nil nil t))
;;   (add-to-list 'cdlatex-command-alist '("pmat" "Insert pmatrix env"
;;                                        "\\begin{pmatrix} ? \\end{pmatrix}"
;;                                        lazytab-position-cursor-and-edit
;;                                        nil nil t))
;;   (add-to-list 'cdlatex-command-alist '("tbl" "Insert table"
;;                                         "\\begin{table}\n\\centering ? \\caption{}\n\\end{table}\n"
;;                                        lazytab-position-cursor-and-edit
;;                                        nil t nil))
;;   :config
;;   ;; Tab handling in org tables
;;   (defun lazytab-position-cursor-and-edit ()
;;     ;; (if (search-backward "\?" (- (point) 100) t)
;;     ;;     (delete-char 1))
;;     (cdlatex-position-cursor)
;;     (lazytab-orgtbl-edit))

;;   (defun lazytab-orgtbl-edit ()
;;     (advice-add 'orgtbl-ctrl-c-ctrl-c :after #'lazytab-orgtbl-replace)
;;     (orgtbl-mode 1)
;;     (open-line 1)
;;     (insert "\n|"))

;;   (defun lazytab-orgtbl-replace (_)
;;     (interactive "P")
;;     (unless (org-at-table-p) (user-error "Not at a table"))
;;     (let* ((table (org-table-to-lisp))
;;            params
;;            (replacement-table
;;             (if (texmathp)
;;                 (lazytab-orgtbl-to-amsmath table params)
;;               (orgtbl-to-latex table params))))
;;       (kill-region (org-table-begin) (org-table-end))
;;       (open-line 1)
;;       (push-mark)
;;       (insert replacement-table)
;;       (align-regexp (region-beginning) (region-end) "\\([:space:]*\\)& ")
;;       (orgtbl-mode -1)
;;       (advice-remove 'orgtbl-ctrl-c-ctrl-c #'lazytab-orgtbl-replace)))

;;   (defun lazytab-orgtbl-to-amsmath (table params)
;;     (orgtbl-to-generic
;;      table
;;      (org-combine-plists
;;       '(:splice t
;;                 :lstart ""
;;                 :lend " \\\\"
;;                 :sep " & "
;;                 :hline nil
;;                 :llend "")
;;       params)))

;;   (defun lazytab-cdlatex-or-orgtbl-next-field ()
;;     (when (and (bound-and-true-p orgtbl-mode)
;;                (org-table-p)
;;                (looking-at "[[:space:]]*\\(?:|\\|$\\)")
;;                (let ((s (thing-at-point 'sexp)))
;;                  (not (and s (assoc s cdlatex-command-alist-comb)))))
;;       (call-interactively #'org-table-next-field)
;;       t))

;;   (defun lazytab-org-table-next-field-maybe ()
;;     (interactive)
;;     (if (bound-and-true-p cdlatex-mode)
;;         (cdlatex-tab)
;;       (org-table-next-field))))
