;;; headerline.el --- Minimal Headerline -*- lexical-binding: t; -*-

(defun org-outline-path-headerline ()
  (propertize
   (substring-no-properties (org-display-outline-path nil t " / " t))
   'face 'org-macro
   )
  )

(define-minor-mode my/org-header-outline-path-mode
  "Show outline path in header line."
  :ligher nil
  (if my/org-header-outline-path-mode
      (setq header-line-format '((:eval (org-outline-path-headerline))))
    (setq mode-line-format '((:eval (org-outline-path-headerline)))))
  )

(add-hook 'org-mode-hook #'my/org-header-outline-path-mode)

(provide 'headerline)

;;; headerline.el ends here
