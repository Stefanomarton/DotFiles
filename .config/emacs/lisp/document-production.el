;;; document-production.el --- document production configuration

;; Common fast pdf viewer inside emacs
(use-package pdf-tools
  :after (LaTeX-mode markdown-mode org-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page) ; Fit page width
  (setq pdf-annot-activate-created-annotations t) ; Enable annotations
  (define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
  (define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
  (define-key pdf-view-mode-map (kbd "h") 'image-backward-hscroll)
  (define-key pdf-view-mode-map (kbd "l") 'image-forward-hscroll)
  (define-key pdf-view-mode-map (kbd "g") 'pdf-view-first-page)
  (define-key pdf-view-mode-map (kbd "G") 'pdf-view-last-page)
  :init
  (pdf-tools-install)
  )

;; Org mode configuration
;; (use-package org-bullets
;;   :after (org-mode org-roam)
;;   :config

;;   (setq org-bullets-face-name (quote org-bullet-face))
;;   (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;;   (setq org-bullets-bullet-list '("✡" "⎈" "✽" "✲" "✱" "✻" "✼" "✽" "✾" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇"))
;;   (setq org-hide-emphasis-markers t)
;;   (custom-set-faces
;;    '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
;;    '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
;;    '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
;;    '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
;;    '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
;;   (setq org-agenda-files '("~/org"))
;;   :init
;;   (org-bullets-mode 1))

(use-package org
  :after dashboard
  :straight t
  :ensure nil
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-export-headline-levels 4)
  (setq org-export-preserve-breaks t)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (add-to-list 'org-file-apps '("\\.pdf" . "zathura %s"))
  (advice-add 'org-latex-compile :after #'delete-file)
  ;; (setq org-latex-pdf-process
  ;;       '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
  ;; (setq org-latex-to-pdf-process
  ;;       '("xelatex -interaction nonstopmode %f"
  ;;         "xelatex -interaction nonstopmode %f")) ;; for multiple passes
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil))

  (add-to-list 'org-latex-classes
               '("report"
                 "\\documentclass[a4paper,11pt,titlepage]{report}
\\usepackage[utf8]{inputenc}
\\usepackage[margin=3cm]{geometry}
\\usepackage[T1]{fontenc}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\setlength{\\parindent}{0pt}
\\usepackage[final]{hyperref} % adds hyper links inside the generated pdf file
\\usepackage{mhchem}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{mathpazo}
\\usepackage{color}
\\usepackage{enumerate}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\tolerance=1000
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.1}
\\hypersetup{pdfborder=0 0 0}"
                 ("\\chapter{%s}" . "\\chapter*{%s}")
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))


  (add-to-list 'org-latex-classes
               '("article"
                 "\\documentclass[11pt,a4paper]{article}
\\setlength{\\parindent}{0pt}
\\usepackage{mhchem}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage[final]{hyperref} % adds hyper links inside the generated pdf file
\\hypersetup{
	colorlinks=true,       % false: boxed links; true: colored links
}
\\usepackage{fixltx2e}
\\usepackage{graphicx}
\\usepackage{longtable}
\\usepackage{float}
\\usepackage[margin=2.5cm]{geometry}
\\usepackage{wrapfig}
\\usepackage{rotating}
\\usepackage[normalem]{ulem}
\\usepackage{amsmath}
\\usepackage{textcomp}
\\usepackage{marvosym}
\\usepackage{wasysym}
\\usepackage{amssymb}
\\usepackage{hyperref}
\\usepackage{mathpazo}
\\usepackage{color}
\\usepackage{enumerate}
\\definecolor{bg}{rgb}{0.95,0.95,0.95}
\\tolerance=1000
      [NO-DEFAULT-PACKAGES]
      [PACKAGES]
      [EXTRA]
\\linespread{1.1}
\\hypersetup{pdfborder=0 0 0}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")))


  (add-to-list 'org-latex-classes '("ebook"
                                    "\\documentclass[11pt, oneside]{memoir}
\\setstocksize{9in}{6in}
\\settrimmedsize{\\stockheight}{\\stockwidth}{*}
\\setlrmarginsandblock{2cm}{2cm}{*} % Left and right margin
\\setulmarginsandblock{2cm}{2cm}{*} % Upper and lower margin
\\checkandfixthelayout
% Much more laTeX code omitted
"
                                    ("\\chapter{%s}" . "\\chapter*{%s}")
                                    ("\\section{%s}" . "\\section*{%s}")
                                    ("\\subsection{%s}" . "\\subsection*{%s}")))

  :init
  (setq org-startup-folded t)
  (setq org-pretty-entities t)
  )

(use-package org-download
  :after dashboard
  :custom
  (org-download-method 'directory)
  (org-download-image-dir "attachments")
  (org-download-heading-lvl nil))

;; (use-package org-fragtog
;;   :hook
;;   (org-mode . org-fragtog-mode)
;;   :after org-mode
;;   )

(use-package org-roam
  :after dashboard
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . consult-org-roam-file-find)
         ("C-c n s" . consult-org-roam-search)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :custom
  (org-pretty-entities t)
  :config
  (setq org-pretty-entities t)
  (setq org-roam-directory "~/GoogleDrive/org")
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  :init
  (setq org-startup-folded t)
  (setq org-pretty-entities t)
  )

(use-package org-roam-ui
  :after org-roam
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-modern
  :config
  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-roam-mode-hook #'org-modern-mode))

(use-package markdown-mode
  :after dashboard
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
  :commands latex-mode
  :straight auctex
  :config
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
  :commands (yas-minor-mode) ; autoload `yasnippet' when `yas-minor-mode' is called
                                        ; using any means: via a hook or by user
                                        ; Feel free to add more commands to this
                                        ; list to suit your needs.
  :hook
  (prog-mode . yas-minor-mode)
  (LaTeX-mode . yas-minor-mode)
  (markdown-mode . yas-minor-mode)
  (laas-mode . yas-minor-mode)
  :config ; stuff to do after requiring the package
  (setq yas-triggers-in-field t)
  (setq yas-snippet-dirs '("~/.config/emacs/snippets"))
  (progn
    (yas-reload-all)))

(use-package warnings
  :config
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

(use-package aas
  :hook
  (org-mode . aas-activate-for-major-mode)
  (markdown-mode . aas-activate-for-major-mode)
  (LaTeX-mode . aas-activate-for-major-mode)
  :config
  (aas-set-snippets 'latex-mode
    "jf" (lambda () (interactive)
	       (yas-expand-snippet "\\\\($1\\\\) $0"))
    "kd" (lambda () (interactive)
	       (yas-expand-snippet "\\[ \n $1 \n \\] \n \n $0")))
  (aas-set-snippets 'org-mode
    "jf" (lambda () (interactive)
	       (yas-expand-snippet "\\\\( $1 \\\\) $0"))
    "kd" (lambda () (interactive)
	       (yas-expand-snippet "\\[ \n $1 \n \\] \n \n $0")))
  (aas-set-snippets 'markdown-mode
    "jf" (lambda () (interactive)
	       (yas-expand-snippet "$$1$ $0"))
    "kd" (lambda () (interactive)
	       (yas-expand-snippet "$$ \n $1 \n $$ \n \n $0"))))

(use-package laas
  :straight (laas :type git :host github :repo "Stefanomarton/LaTeX-auto-activating-snippets")
  :hook
  (LaTeX-mode . laas-mode)
  (markdown-mode . laas-mode)
  (org-mode . laas-mode)
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
    "inti" (lambda () (interactive)
	         (yas-expand-snippet "\\int"))
    "intd" (lambda () (interactive)
	         (yas-expand-snippet "\\int_{$1}^{$2} $0"))
    "df" (lambda () (interactive)
	       (yas-expand-snippet "_{$1}$0"))
    "rt" (lambda () (interactive)
	       (yas-expand-snippet "^{$1}$0"))
    ;; add accent snippets
    :cond #'laas-object-on-left-condition
    "qq" (lambda () (interactive) (laas-wrap-previous-object "sqrt"))))

(use-package cdlatex
  :commands latex-mode
  :hook (LaTeX-mode . cdlatex-mode)
  :custom
  (cdlatex-takeover-dollar nil)
  (cdlatex-math-modify-prefix 58))

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

(use-package languagetool
  :after dashboard
  :defer t
  :commands (languagetool-check
             languagetool-clear-suggestions
             languagetool-correct-at-point
             languagetool-correct-buffer
             languagetool-set-language
             languagetool-server-mode
             languagetool-server-start
             languagetool-server-stop)
  :config
  (setq languagetool-java-arguments '("-Dfile.encoding=UTF-8"
                                      "-cp" "/usr/share/languagetool:/usr/share/java/languagetool/*")
        languagetool-console-command "org.languagetool.commandline.Main"
        languagetool-server-command "org.languagetool.server.HTTPServer")
  )

(provide 'document-production)

;;; document-production.el ends here
