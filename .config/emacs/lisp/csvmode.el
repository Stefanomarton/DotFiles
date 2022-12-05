(provide 'csvmode)

(use-package csv-mode
  :hook (csv-mode . lc/init-csv-mode)
  :general
  (lc/local-leader-keys
   :keymaps 'csv-mode-map
   :states 'normal
   "a" '(csv-align-fields :wk "align fields")
   "A" '(lc/csv-align-visible :wk "align fields, visible")
   "i"  '(lc/init-csv-mode :wk "init csv mode")
   "u" '(csv-unalign-fields :wk "unalign fields")
   "s" '(csv-sort-fields :wk "sort fields")
   ";" '(lc/set-csv-semicolon-separator :wk "set semicolon sep")
   "," '(lc/reset-csv-separators :wk "set comma sep"))
  :init
  (defun lc/csv-align-visible (&optional arg)
    "Align visible fields"
    (interactive "P")
    (csv-align-fields nil (window-start) (window-end)))
  (defun lc/set-csv-semicolon-separator ()
    (interactive)
    (customize-set-variable 'csv-separators '(";")))
  (defun lc/reset-csv-separators ()
    (interactive)
    (customize-set-variable 'csv-separators lc/default-csv-separators))
  (defun lc/init-csv-mode ()
    (interactive)
    (lc/set-csv-separators)
    (lc/csv-highlight)
    (call-interactively 'csv-align-fields))
  :config
  (require 'cl)
  (require 'color)
  (defun lc/set-csv-separators ()
    (interactive)
    (let* ((n-commas (count-matches "," (point-at-bol) (point-at-eol)))
           (n-semicolons (count-matches ";" (point-at-bol) (point-at-eol))))
      (if ( ; <
           > n-commas n-semicolons)
          (customize-set-variable 'csv-separators '("," " "))
        (customize-set-variable 'csv-separators '(";" " ")))))
  (defun lc/csv-highlight ()
    (interactive)
    (font-lock-mode 1)
    (let* ((separator (string-to-char (car csv-separators)))
           (n (count-matches (string separator) (point-at-bol) (point-at-eol)))
           (colors (loop for i from 0 to 1.0 by (/ 2.0 n)
                         collect (apply #'color-rgb-to-hex
                                        (color-hsl-to-rgb i 0.3 0.5)))))
      (loop for i from 2 to n by 2
            for c in colors
            for r = (format "^\\([^%c\n]+%c\\)\\{%d\\}" separator separator i)
            do (font-lock-add-keywords nil `((,r (1 '(face (:foreground ,c)))))))))
  )
