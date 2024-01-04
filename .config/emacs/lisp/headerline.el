;;; headerline.el --- Minimal Headerline -*- lexical-binding: t; -*-

(defface org-outline-path-headerline-face
  '((t (:foreground "white" :weight ultra-bold)))
  "face for headerline outline in org-mode")

(defun org-outline-path-headerline ()
  (propertize
   (substring-no-properties (org-display-outline-path nil t " / " t))
   'face 'org-outline-path-headerline-face
   )
  )

(define-minor-mode my/org-header-outline-path-mode
  "Show outline path in header line."
  :ligher nil
  (if my/org-header-outline-path-mode
      (setq header-line-format '((:eval (org-outline-path-headerline))))
    )
  )

(add-hook 'org-mode-hook #'my/org-header-outline-path-mode)

;; (defvar-local org+-header-line-format nil
;;   "Cons with position of last synchronization of outline path
;; and the header line string.")

;; (defun org+-list-mapconcat (fun list separator)
;;   "Replace each non-nil cdr SUBLIST of LIST with `(,(FUN SEPARATOR) . SUBLIST)."
;;   (setq list (mapcar fun list))
;;   (let ((sublist list))
;;     (while (cdr sublist)
;;       (setcdr sublist (cons separator (cdr sublist)))
;;       (setq sublist (cddr sublist))))
;;   list)
;; ;; Test: (assert (equal (org+-list-mapconcat #'1+ '(1 2 3) 0) '(2 0 3 0 4)) "Outch.")
;; ;; (assert (equal (org+-list-mapconcat #'identity '(1) 0) '(1)) "Outch.")

;; (defun org+-get-outline-path (&optional maxdepth)
;;   "Return the outline path of the current entry.
;; MAXDEPTH defaults to 10."
;;   (unless (numberp maxdepth)
;;     (setq maxdepth 10))
;;   (save-excursion
;;     (let (ret)
;;       (while (and
;;           (numberp maxdepth)
;;           (>= maxdepth 1)
;;           (re-search-backward (format "^\\*\\{1,%d\\} " maxdepth) nil t))
;;     (let* ((components (org-heading-components))
;;            (level (car components))
;;            (txt (nth 4 components)))
;;       (setq maxdepth (1- level))
;;       (push (cons txt (point)) ret)))
;;       ret)))

;; (defun org+-header-line ()
;;   "Outline path in header line of Org."
;;   (let ((pos (car org+-header-line-format))
;;     (pt (point)))
;;     (when (or (null (number-or-marker-p pos))
;;           (save-excursion
;;         (goto-char (min pos pt))
;;         (re-search-forward "^\\*+ " (max pos pt) t)))
;;       (setq org+-header-line-format
;;         (cons (point)
;;           (append
;;            (list " ")
;;            (org+-list-mapconcat
;;             (lambda (txt-pos)
;;               (list :propertize
;;                 (car txt-pos)
;;                 'local-map
;;                 (let ((map (make-sparse-keymap))
;;                   (pos (set-marker (make-marker) (cdr txt-pos))))
;;                   (define-key map [header-line mouse-1]
;;                 (lambda ()
;;                   (interactive)
;;                   (switch-to-buffer (marker-buffer pos))
;;                   (goto-char pos)))
;;                   map))
;;               )
;;             (org+-get-outline-path)
;;             " ‖ "))))))
;;   (cdr org+-header-line-format))

;; (defvar-local org+-heder-old-header nil
;;   "Old value of `header-line-format'.")

;; (define-minor-mode org+-header-outline-path-mode
;;   "Show outline path in header line."
;;   :ligher nil
;;   (if org+-header-outline-path-mode
;;       (setq org+-header-old-header header-line-format
;;         header-line-format '((:eval (org+-header-line))))
;;     (setq header-line-format org+-header-old-header)))

;; (add-hook 'org-mode-hook #'org+-header-outline-path-mode)

(provide 'headerline)

;;; headerline.el ends here
