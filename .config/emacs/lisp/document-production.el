;;; document-production.el --- document production configuration -*- lexical-binding: t; -*-

;; Common fast pdf viewer inside emacs
(use-package pdf-tools
  :after (LaTeX-mode markdown-mode org)
  :config
  (setq-default pdf-view-display-size 'fit-page) ; Fit page width
  (setq pdf-annot-activate-created-annotations t) ; Enable annotations
  (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "h") 'image-backward-hscroll)
  (define-key pdf-view-mode-map (kbd "l") 'image-forward-hscroll)
  (define-key pdf-view-mode-map (kbd "g") 'pdf-view-first-page)
  (define-key pdf-view-mode-map (kbd "G") 'pdf-view-last-page)
  (pdf-tools-install)
  )

(use-package markdown-mode
  :mode ("\\.md?\\'" . markdown-mode)
  :custom
  (setq markdown-fontify-code-blocks-natively t)
  (setq markdown-enable-math t)
  :config

  (defun export-buffer-to-pdf ()
    "Export the current Markdown buffer to PDF using Pandoc with conditional flags."
    (interactive)
    (let* ((md-file (buffer-file-name))
           (output-file (concat (file-name-sans-extension md-file) ".pdf"))
           (default-directory (file-name-directory md-file))
           (config-file (concat default-directory "config.yaml"))
           (template-file (concat default-directory "template.latex"))
           (metadata-flag (if (file-exists-p config-file) (format "--metadata-file=%s" config-file) ""))
           (template-flag (if (file-exists-p template-file) (format "--template=%s" template-file) ""))
           (pandoc-command (format "pandoc -s %s %s %s -o %s --pdf-engine=xelatex"
                                   md-file metadata-flag template-flag output-file)))

      (message "Exporting Markdown file to PDF")
      (start-process-shell-command "pandoc-export" nil pandoc-command)))

  (defun open-pdf-with-zathura ()
    "Open the PDF file associated with the current buffer in Zathura."
    (interactive)
    (let ((pdf-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
      (start-process "zathura" nil "zathura" pdf-file)))

  (defun open-pdf-with-pdf-tools ()
    "Open the PDF file associated with the current buffer in pdf-tools."
    (interactive)
    (let ((pdf-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf")))
      (if (file-exists-p pdf-file)
	      (progn
	        (pdf-tools-install)
	        (find-file pdf-file))
        (message "PDF file not found."))))

  (evil-define-key 'normal markdown-mode-map
    (kbd "<leader>ee") 'export-buffer-to-pdf
    (kbd "<leader>ez") 'open-pdf-with-zathura
    (kbd "<leader>ep") 'open-pdf-with-pdf-tools)

  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-enable-math t))

(use-package tex
  :straight auctex
  :config
  (add-to-list 'major-mode-remap-alist '(latex-mode . LaTeX-mode))

  (setq TeX-save-query nil
	    TeX-clean-confirm nil
	    TeX-command-default "XeLaTeX"
	    TeX-source-correlate-start-server t
	    TeX-source-correlate-method 'synctex)
  (TeX-source-correlate-mode 1)
  (add-to-list 'TeX-view-program-selection
	           '(output-pdf "Zathura"))

  (defun my-export-to-pdf ()
    "Export the current LaTeX document to PDF using AUCTeX."
    (interactive)
    (TeX-command "LaTeX" 'TeX-master-file nil)
    (TeX-clean))

  (defun my-export-to-pdf-and-view ()
    "Export the current LaTeX document to PDF using AUCTeX."
    (interactive)
    (TeX-command "LaTeX" 'TeX-master-file nil)
    (TeX-clean)
    (TeX-view)
    )

  ;; Toggle between master and current compilation
  (defvar my-latex-original-master nil
    "Variable to store the original value of TeX-master.")

  (defun my-latex-toggle-command ()
    "Toggle between executing commands on master and current file."
    (interactive)
    (if my-latex-original-master
	    (progn
	      (setq TeX-master my-latex-original-master)
	      (setq my-latex-original-master nil))
      (progn
	    (setq my-latex-original-master TeX-master)
	    (setq TeX-master nil)))
    (message "Switched command: %s" (if TeX-master "master" "current")))

  (evil-define-key 'normal LaTeX-mode-map
    (kbd "<leader> ee") 'my-export-to-pdf
    (kbd "C-c T") 'my-latex-toggle-command
    (kbd "C-c E") 'my-export-to-pdf-view
    (kbd "C-c t") 'lsp-ui-imenu)

  (defun my-select-frac ()
    "Select the \\frac command and move to the start of the nearest \\frac."
    (interactive)
    (let ((current-point (point))
          (frac-start nil))
      (save-excursion
	    (when (re-search-backward "\\\\frac" nil t)
          (setq frac-start (match-beginning 0))
          (when (and (<= frac-start current-point) (<= current-point (match-end 0)))
            (setq current-point frac-start)
            (setq frac-start nil))))
      (if frac-start
          (goto-char frac-start)
	    (message "No \\frac found")))
    (when (looking-at "\\\\frac")
      (let ((start (point))
            (end (progn
                   (search-forward "{")
                   (backward-char)
                   (let ((level 1))
                     (while (> level 0)
                       (search-forward-regexp "{\\|}" nil t)
                       (if (string= (match-string 0) "{")
                           (setq level (1+ level))
			             (setq level (1- level)))))
                   (backward-char)
                   (point))))
	    (set-mark start)
	    (goto-char end))))

  ;; (global-set-key (kbd "C-c f") 'my-select-frac)
  (evil-define-key 'normal LaTeX-mode-map (kbd "<leader>r") 'my-select-frac)

  :init
  (add-hook 'LaTeX-mode-hook 'prettify-symbols-mode))

(use-package yasnippet
  :defer t
  :commands (yas-minor-mode)
  :hook
  (text-mode . yas-minor-mode)
  (prog-mode . yas-minor-mode)
  (LaTeX-mode . yas-minor-mode)
  (markdown-mode . yas-minor-mode)
  (org-mode . yas-minor-mode)
  (yas-minor-mode . yas-reload-all)
  :config
  (yas-global-mode 1)
  (setq yas-triggers-in-field t)
  (setq yas-snippet-dirs '("~/.config/emacs/snippets")))

(use-package warnings
  :config
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

(use-package aas
  :hook
  (org-mode . aas-activate-for-major-mode)
  (markdown-mode . aas-activate-for-major-mode)
  (LaTeX-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'LaTeX-mode
    "jf" (lambda () (interactive)
	       (yas-expand-snippet "\\\\($1\\\\) $0"))
    "jc" (lambda () (interactive)
	       (yas-expand-snippet "\\\\(\\ce{ $1 }\\\\) $0"))
    "kd  " (lambda () (interactive)
	         (yas-expand-snippet "\\[ \n $1 \n \\] \n \n $0")))
  (aas-set-snippets 'org-mode
    "jf" (lambda () (interactive)
	       (yas-expand-snippet "\\\\( $1 \\\\) $0"))
    "jc" (lambda () (interactive)
	       (yas-expand-snippet "\\\\(\\ce{ $1 }\\\\) $0"))
    "kd" (lambda () (interactive)
	       (yas-expand-snippet "\\[ \n $1 \n \\] \n \n $0")))
  (aas-set-snippets 'markdown-mode
    "jf" (lambda () (interactive)
	       (yas-expand-snippet "$ $1$ $0 $"))
    "jc" (lambda () (interactive)
	       (yas-expand-snippet "\\\\(\\ce{ $1 }\\\\) $0"))
    "kd" (lambda () (interactive)
	       (yas-expand-snippet "$$ \n $1 \n $$ \n \n $0"))))

(use-package laas
  :straight (laas :type git :host github :repo "Stefanomarton/LaTeX-auto-activating-snippets")
  :hook
  (LaTeX-mode . laas-mode)
  (markdown-mode . laas-mode)
  (org-mode . laas-mode)
  :config
  (aas-set-snippets 'laas-mode
    ;; set condition!
    :cond #'texmathp ; expand only while in math

    ",t" (lambda () (interactive)
	       (yas-expand-snippet "\\int"))

    ".." (lambda () (interactive)
	       (yas-expand-snippet "_{$1}$0"))
    "ds" (lambda () (interactive)
	       (yas-expand-snippet "\\Delta_{$1}S $0"))
    "dh" (lambda () (interactive)
	       (yas-expand-snippet "\\Delta_{$1}H $0"))
    "dg" (lambda () (interactive)
	       (yas-expand-snippet "\\Delta_{$1}G $0"))

    ;; positive apices
    ",," (lambda () (interactive)
	       (yas-expand-snippet "^{$1} $0"))
    ",x" (lambda () (interactive)
	       (yas-expand-snippet "^{1} $0"))
    ",c" (lambda () (interactive)
	       (yas-expand-snippet "^{2} $0"))
    ",v" (lambda () (interactive)
	       (yas-expand-snippet "^{3} $0"))
    ",s" (lambda () (interactive)
	       (yas-expand-snippet "^{4} $0"))
    ",d" (lambda () (interactive)
	       (yas-expand-snippet "^{5} $0"))
    ",f" (lambda () (interactive)
	       (yas-expand-snippet "^{6} $0"))
    ",w" (lambda () (interactive)
	       (yas-expand-snippet "^{7} $0"))
    ",e" (lambda () (interactive)
	       (yas-expand-snippet "^{8} $0"))
    ",r" (lambda () (interactive)
	       (yas-expand-snippet "^{9} $0"))

    ;; negative apices
    ".." (lambda () (interactive)
	       (yas-expand-snippet "^{-$1} $0"))
    ".x" (lambda () (interactive)
	       (yas-expand-snippet "^{-1} $0"))
    ".c" (lambda () (interactive)
	       (yas-expand-snippet "^{-2} $0"))
    ".v" (lambda () (interactive)
	       (yas-expand-snippet "^{-3} $0"))
    ".s" (lambda () (interactive)
	       (yas-expand-snippet "^{-4} $0"))
    ".d" (lambda () (interactive)
	       (yas-expand-snippet "^{-5} $0"))
    ".f" (lambda () (interactive)
	       (yas-expand-snippet "^{-6} $0"))
    ".w" (lambda () (interactive)
	       (yas-expand-snippet "^{-7} $0"))
    ".e" (lambda () (interactive)
	       (yas-expand-snippet "^{-8} $0"))
    ".r" (lambda () (interactive)
	       (yas-expand-snippet "^{-9} $0"))

    ".," (lambda () (interactive)
	       (yas-expand-snippet "^{$1}_{$0}"))

    "kk" (lambda () (interactive)
	       (yas-expand-snippet "_{$1} $0"))
    ;; add accent snippets
    :cond #'laas-object-on-left-condition
    ".q" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))
    ".v" (lambda () (interactive) (laas-wrap-previous-object "vec"))
    ".t" (lambda () (interactive) (laas-wrap-previous-object "text"))
    ".b" (lambda () (interactive) (laas-wrap-previous-object "mathbf"))))

(use-package cdlatex
  ;; :commands latex-mode
  :hook (LaTeX-mode . cdlatex-mode)
  :custom
  (cdlatex-takeover-dollar nil)
  (cdlatex-math-modify-prefix ?~)
  ;; (cdlatex-math-symbol-prefix nil)
  )

(use-package latex-table-wizard
  :commands latex-mode
  :config
  (defun some-useful-name (stuff-to-configure)
    "Some useful documentation here!."
    (dolist (entry stuff-to-configure)
      (add-to-list 'latex-table-wizard-transient-keys
		           (cons (intern (concat "latex-table-wizard-" (symbol-name (car entry))))
			             (cdr entry)))))

  ;; example use
  (some-useful-name '((right . "l")
		              (left . "h")
		              (beginning-of-cell . "ii")
		              (down . "j")
		              (up . "k")
		              (end-of-cell . "a")
		              (beginning-of-row . "II")
		              (end-of-row . "A")
		              (bottom . "G")
		              (top . "gg")
		              (mark-cell . "m")
		              (insert-column . "C")
		              (insert-row .	"R")
		              (kill-column-content ."DCC"	)
		              (kill-row-content . "DRC"	)
		              (delete-column . "Dc"	)
		              (delete-row . "Dr"	)
		              )))

(use-package jinx
  :straight (:host github :repo "minad/jinx")
  :bind (:map evil-normal-state-map
              ("<leader>j" . jinx-correct)
              ("<leader>J" . jinx-correct-all))
  :config
  (setq jinx-languages "it_IT, en_US")
  )

(provide 'document-production)

;;; document-production.el ends here
