;;; modeline.el --- Minimal Modeline

(defface evil-visual-face
  '((t (:foreground "#b48ead")))
  "Face for Evil visual state")

(defface evil-normal-face
  '((t (:foreground "#81a1c1")))
  "Face for Evil normal state")

(defface evil-insert-face
  '((t (:foreground "#ebcb8b")))
  "Face for Evil insert state")

(setq-default
 mode-line-format
 '(
   ;; point position
   (:propertize "    " 'face 'font-lock-keyword-face)
   (8
    ;; (:propertize "  " 'face 'font-lock-keyword-face)
    (:propertize " %l" face font-lock-string-face)
    ;; (:eval (propertize ":%c" 'face (if (>= (current-column) 80)
    ;;                                   'font-lock-warning-face
    ;;                                 'font-lock-string-face)))
    )
   (8
    (:propertize " " 'face 'font-lock-keyword-face)
    (:eval (cond
            ((eq evil-state 'visual) (propertize "\uf111" 'face 'evil-visual-face))
            ((eq evil-state 'normal) (propertize "\uf111" 'face 'evil-normal-face))
            ((eq evil-state 'insert) (propertize "\uf111" 'face 'evil-insert-face))
            (t (propertize "*" 'face 'font-lock-variable-name-face)))))
   ;; (4
   ;;  (:propertize "%m " face font-lock-variable-name-face
   ;; 		 help-echo buffer-file-coding-system))

   ;; (2 (:propertize face font-lock-comment-face))

   ;; shortened directory (if buffer have a corresponding file)
   (
    :eval
    (when (buffer-file-name)
      (propertize (shorten-directory default-directory 35)
		  'face 'font-lock-comment-face)))


   ;; buffer name
   ;; (:propertize "%b" face font-lock-doc-face)

   ;; right aligned stuff
   (:eval
    (let* ((status-offset 10))
      (concat
       ;; nyan-cat
       (concat
	(propertize " " 'display `(space :align-to (- right ,status-offset)))
	)
       (propertize (format-time-string " %H:%M") 'face 'font-lock-keyword-face)))))

 ;; for nyan cat
 ;; (:eval
 ;;  (let* ((status-offset 2)
 ;;         (time-offset (- right 6)) ; Adjust the number (6) for desired spacing
 ;;         (nyan-offset
 ;;          (+ status-offset (if nyan-mode (+ 2 nyan-bar-length) 0))))

 ;;    (concat

 ;; nyan-cat
 ;; (when nyan-mode
 ;;   (concat
 ;;    (propertize " " 'display `(space :align-to (- right ,nyan-offset)))
 ;;    (nyan-create)
 ;;    ))


 ;; ;; read-only / changed
 ;; (propertize " " 'display `(space :align-to (- right ,status-offset)))
 ;; (cond (buffer-read-only
 ;;        (propertize "RO" 'face 'eshell-prompt))
 ;;       ((buffer-modified-p)
 ;;        (propertize "* " 'face 'eshell-prompt))
 ;;       (t "  "))))

 )

(defun special-buffer-p (buffer-name)
  "Check if buffer-name is the name of a special buffer."
  (or (string-match-p "^\\*.+\\*$" buffer-name)
      ;; workaround for magit's 'trailing asterisk' problem
      ;; https://github.com/magit/magit/issues/2334
      (string-match-p "^\\*magit.*:.+$" buffer-name)))

;; helper function
;; stolen from: http://amitp.blogspot.se/2011/08/emacs-custom-mode-line.html
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

'(:eval (propertize
         " " 'display
         `((space :align-to (- (+ right right-fringe right-margin)
			       ,(+ 3 (string-width mode-name)))))))


(provide 'modeline)

;;; modeline.el ends here
