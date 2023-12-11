;;; orgconfig.el --- org-mode configuration -*- lexical-binding: t; -*-

(use-package org
  :defer t
  :after dashboard
  :straight t
  :ensure nil
  :hook
  (org-mode . org-cdlatex-mode)
  :custom
  (org-use-speed-commands t)
  (org-src-fontify-natively t)
  (org-adapt-indentation t)
  (org-list-allow-alphabetical t)
  ;; (org-cite-global-bibliography )
  (org-highlight-latex-and-relatex 'native)
  :config

  (setq org-latex-to-mathml-convert-command
        "latexmlmath \"%i\" --presentationmathml=%o")

  (defun my/org-time-stamp ()
    (interactive)
    (org-timestamp '(16) nil)
    )

  (evil-define-key 'normal org-mode-map (kbd "gt") 'my/org-time-stamp)

  (defun my/org-mode/load-prettify-symbols ()
    (interactive)
    (setq prettify-symbols-alist
          '(("\\\\" . ?↩)
            )))
  (add-hook 'org-mode-hook 'my/org-mode/load-prettify-symbols)

  (setq org-agenda-files (directory-files-recursively "~/GoogleDrive/org" "\\.org$"))
  (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-export-headline-levels 6)
  (setq org-export-preserve-breaks nil) ;; preserve newline in exports

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5)) ;; fix dimension of latex fragments

  (add-to-list 'org-file-apps '("\\.pdf" . "zathura %s")) ;; open pdf files with zathura
  (advice-add 'org-latex-compile :after #'delete-file) ;; delete compilation files after .tex export

  ;; TODO: da sistemare
  ;; (setq org-publish-project-alist
  ;;       '(
  ;;         (
  ;;          "roam"
  ;;          :base-directory "~/GoogleDrive/org/uni"
  ;;          :publishing-directory "~/GoogleDrive/org/pdf/uni"
  ;;          :publishing-function org-latex-publish-to-pdf
  ;;          :base-extension "org$"
  ;;          :recursive t
  ;;          )
  ;;         )
  ;;       )

  ;; modify export folder for org export
  ;; taken from https://stackoverflow.com/questions/9559753/emacs-org-mode-export-to-another-directory

  (defun org-export-output-file-name-modified (orig-fun extension &optional subtreep pub-dir)
    (unless pub-dir
      (setq pub-dir "~/GoogleDrive/org/pdf")
      (unless (file-directory-p pub-dir)
        (make-directory pub-dir)))
    (apply orig-fun extension subtreep pub-dir nil))
  (advice-add 'org-export-output-file-name :around #'org-export-output-file-name-modified)


  ;; Double compilation for TOC
  :init
  (setq org-latex-default-class "report")
  (setq org-startup-folded t)
  (setq org-pretty-entities t)
  (setq org-pretty-entities-include-sub-superscripts nil)
  )

(use-package ox-latex
  :straight nil
  :ensure nil
  :after ox
  :config
  (setq org-latex-pdf-process
        '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
          "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

  (unless (boundp 'org-latex-classes)
    (setq org-latex-classes nil))

  (setq org-latex-default-class "report")
  (add-to-list 'org-latex-classes
               '("report"
                 "\\documentclass[a4paper,11pt,titlepage]{report}
                 \\hbadness 99999
                 \\usepackage[marginal]{footmisc} % cleaner footnotes
                 \\usepackage[utf8]{inputenc}
                 \\usepackage[margin=3cm]{geometry}
                 \\usepackage[T1]{fontenc}
                 \\usepackage{fixltx2e}
                 \\usepackage{graphicx}
                 \\usepackage{longtable}
                 \\usepackage{float}
                 \\usepackage{wrapfig}
                 \\usepackage{rotating}
                 \\usepackage{cancel}
                 \\setlength{\\parskip}{1pt}
                 \\usepackage{parskip}
                 \\usepackage[final]{hyperref} % adds hyper links inside the generated pdf file
                 \\usepackage{mhchem}
                 \\usepackage[normalem]{ulem}
                 \\usepackage{amsmath}

                 \\usepackage{mathtools}
                 \\DeclarePairedDelimiter\\bra{\\langle}{\\rvert}
                 \\DeclarePairedDelimiter\\ket{\\lvert}{\\rangle}
                 \\DeclarePairedDelimiterX\\braket[2]{\\langle}{\\rangle}{#1\\,\\delimsize\\vert\\,\\mathopen{}#2}

                 \\usepackage{textcomp}
                 \\usepackage{marvosym}
                 \\usepackage{wasysym}
                 \\usepackage{amssymb}
                 \\usepackage{hyperref}
                 \\hypersetup{
                     colorlinks=true,       % false: boxed links; true: colored links
                     linkcolor=blue,        % color of internal links
                     citecolor=blue,        % color of links to bibliography
                     filecolor=blue,     % color of file links
                     urlcolor=blue
                 }
                 \\usepackage{mathpazo}
                 \\usepackage{color}
                 \\definecolor{bg}{rgb}{0.95,0.95,0.95}
                 % Define cool colorboxes
                 \\usepackage[most]{tcolorbox}
                 \\newtcolorbox{bx}{
                    enhanced,
                    boxrule=0pt,frame hidden,
                    borderline west={4pt}{0pt}{black},
                    colback=black!5!white,
                    sharp corners,
                 }
                 \\usepackage{enumitem}
                 \\setlist{noitemsep}
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
  )

(use-package ox-hugo
  :after ox)

(use-package org-download
  :defer t
  :commands (org-download-clipboard)
  ;; :after (org-roam org)
  :init
  (setq org-download-display-inline-images 'posframe)
  ;; (setq org-download-timestamp "")
  (setq org-download-method 'directory)
  ;; (setq org-download-image-org-width 7)
  (setq org-download-image-latex-width 7)
  (setq org-download-heading-lvl nil)
  (defun custom/org-download-dir ()
    "Download files in ./attachments/$filename/"
    (setq-local org-download-image-dir (concat
    			                        "./attachments/"
    			                        (file-name-sans-extension (buffer-name))
    			                        "/")
                )                                                                    ; Store downloads in ./resources/%filename/
    )                                                                                ; relative to the .org file
  (add-hook 'org-mode-hook 'custom/org-download-dir)
  (add-hook 'org-roam-mode-hook 'custom/org-download-dir)

  :config
  ;; Modify function to avoid writing useless comment
  (defun my-org-download-annotate-default (link)
    "Annotate LINK with the time of download."
    (format ""
            (if (equal link org-download-screenshot-file)
                "screenshot"
              link)
            (format-time-string "%Y-%m-%d %H:%M:%S")))
  (setq org-download-annotate-function 'my-org-download-annotate-default)
  )


(use-package org-roam
  :defer t
  :commands (org-roam-node-find org-roam-capture consult-notes)
  ;; :after dashboard
  :bind (("<leader>ob" . org-roam-buffer-toggle)
         ("<leader>of" . consult-org-roam-file-find)
         ("<leader>og" . consult-notes-search-in-all-notes)
         ("<leader>oi" . org-roam-node-insert)
         ("<leader>oo" . consult-notes)
         ("<leader>ok" . org-roam-capture)
         ("<leader>oc" . my/org-roam-node-find-courses)
         )
  :config
  ;; configuration for link buffer
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))
               '("\w+\.org" (display-buffer-full-frame)))

  (setq org-pretty-entities t)
  (setq org-roam-directory "~/GoogleDrive/org")

  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))

  (setq org-roam-node-display-template
        (concat "${type:15} ${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)

  :init
  (setq org-roam-capture-templates
        '(("u" "uni" plain
           "%?"
           :if-new (file+head "uni/${slug}.org"
                              "#+title: ${title}\n#+filetags:\n#+date: %U")
           :immediate-finish t
           :unnarrowed t)
          ("c" "course" plain
           "%?"
           :if-new (file+head "uni/courses/${slug}.org"
                              "#+title: ${title}\n#+filetags: %^G\n#+date: %U")
           :immediate-finish t
           :unnarrowed t)
          ("r" "reference" plain "%?"
           :if-new
           (file+head "uni/reference/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("n" "new" plain "%?"
           :if-new
           (file+head "new/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("b" "blog" plain "%?"
           :if-new
           (file+head "blog/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("w" "work" plain "%?"
           :if-new
           (file+head "work/${title}.org" "#+title: ${title}\n#+filetags: %^g :article:\n")
           :immediate-finish t
           :unnarrowed t)))

  (defun my/org-roam-node-find-courses ()
    "Show list of `org-roam-node-find' only under dirA."
    (interactive)
    (org-roam-node-find nil nil
                        (lambda (node)
                          (file-in-directory-p
                           (org-roam-node-file node)
                           (expand-file-name "uni/courses" org-roam-directory)))))
  )

(use-package org-roam-ui
  :defer t
  :after org-roam
  :straight
  (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-modern
  :hook
  (org-mode . org-modern-mode)
  (org-roam-mode . org-modern-mode))

(use-package citar
  :defer t
  :after (org org-roam)
  :init
  (setq citar-bibliography '("~/GoogleDrive/org/uni/lib.bib"))
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package consult-notes
  :after (org org-roam)
  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  :commands (consult-notes
             consult-notes-search-in-all-notes
             ;; I'm using org-roam so:
             consult-notes-org-roam-find-node
             consult-notes-org-roam-find-node-relation)
  :config
  (consult-notes-org-roam-mode)

  ;; Search org-roam notes for citations (depends on citar)
  (defun consult-notes-org-roam-cited (reference)
    "Return a list of notes that cite the REFERENCE."
    (interactive (list (citar-select-ref
                        :rebuild-cache current-prefix-arg
                        :filter (citar-has-note))))
    (let* ((ids
            (org-roam-db-query [:select * :from citations
                                        :where (= cite-key $s1)]
                               (car reference)))
           (anodes
            (mapcar (lambda (id)
                      (org-roam-node-from-id (car id)))
                    ids))
           (template
            (org-roam-node--process-display-format org-roam-node-display-template))
           (bnodes
            (mapcar (lambda (node)
                      (org-roam-node-read--to-candidate node template)) anodes))
           (node (completing-read
                  "Node: "
                  (lambda (string pred action)
                    (if (eq action 'metadata)
                        `(metadata
                          ;; get title using annotation function
                          (annotation-function
                           . ,(lambda (title)
                                (funcall org-roam-node-annotation-function
                                         (get-text-property 0 'node title))))
                          (category . org-roam-node))
                      (complete-with-action action bnodes string pred)))))
           (fnode
            (cdr (assoc node bnodes))))
      (if ids
          ;; Open node in other window
          (org-roam-node-open fnode)
        (message "No notes cite this reference."))))
  :init
  (setq consult-notes-file-dir-sources
        '(("course"             ?c "~/GoogleDrive/org/uni/courses/")))

  )

(provide 'orgconfig)

;;; org-config ends here
