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
  :config
  (setq org-highlight-latex-and-related '(latex script entities))
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
  :straight (:host github :repo "org-roam/org-roam"
                   :files (:defaults "extensions/*"))
  :demand t
  ;; :commands (org-roam-node-find org-roam-capture consult-notes)
  :bind
  (:map evil-normal-state-map
        ("<leader>of" . consult-org-roam-file-find)
        ("<leader>og" . consult-notes-search-in-all-notes)
        ("<leader>oo" . consult-notes)
        ("<leader>on" . consult-notes-org-roam-find-node)
        ("<leader>ok" . org-roam-capture)
        ("<leader>oc" . my/org-roam-node-find-courses))
  (:map org-mode-map
        ("<leader>ob" . org-roam-buffer-toggle)
        ("C-c o i" . org-roam-node-insert)
        )
  :config
  (setq org-roam-mode-sections
        '((org-roam-backlinks-section :unique t)
          org-roam-reflinks-section))
  ;; configuration for link buffer
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-direction)
                 (direction . right)
                 (window-width . 0.33)
                 (window-height . fit-window-to-buffer))
               '("\w+\.org" (display-buffer-full-frame)))

  (setq org-pretty-entities t)
  (setq org-roam-directory (file-truename "~/GoogleDrive/org"))

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


  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; update modified time stamp ;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (add-hook 'org-mode-hook (lambda ()
                             (setq-local time-stamp-active t
                                         time-stamp-line-limit 18
                                         time-stamp-start "^#\\+LAST_MODIFIED: [ \t]*"
                                         time-stamp-end "$"
                                         time-stamp-format "\[%Y-%m-%d %a %H:%M:%S\]")
                             (add-hook 'before-save-hook 'time-stamp nil 'local)))

  ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; customise the slug function
  (defun hiddyn/select-tag ()
    (setq hiddyn/tag-list (sort (completing-read-multiple "Select a tag: " (org-roam-tag-completions)) #'string<))
    (mapconcat 'identity hiddyn/tag-list "_"))

  (defun hiddyn/filetags ()
    (concat ":" (mapconcat 'identity hiddyn/tag-list ":") ":"))

  (setq org-roam-capture-templates
        '(("u" "uni" plain
           "%?"
           :if-new
           (file+head "uni/%<%Y%m%d>--${slug}__%(hiddyn/select-tag).org"
                      "#+title: ${title}\n#+filetags: %(hiddyn/filetags)\n#+CREATED: %U\n#+LAST_MODIFIED: %U")
           :immediate-finish t
           :unnarrowed t)
          ("c" "course" plain
           "%?"
           :if-new (file+head "uni/courses/%<%Y%m%d>--${slug}__%(hiddyn/select-tag).org"
                              "#+title: ${title}\n#+filetags: %(hiddyn/filetags)\n#+CREATED: %U\n#+LAST_MODIFIED: %U")
           :immediate-finish t
           :unnarrowed t)
          ("n" "inbox" plain "%?"
           :if-new
           (file+head "inbox/%<%Y%m%d>--${slug}__%(hiddyn/select-tag).org"
                      "#+title: ${title}\n#+filetags: %(hiddyn/filetags)\n#+CREATED: %U\n#+LAST_MODIFIED: %U")
           :immediate-finish t
           :unnarrowed t)
          ("b" "blog" plain "%?"
           :if-new
           (file+head "blog/${title}.org" "#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)
          ("l" "literature note" plain
           "%?"
           :target
           (file+head
            "%(expand-file-name (or citar-org-roam-subdir \"\") org-roam-directory)/%<%Y%m%d>--${citar-citekey}__%(hiddyn/select-tag).org"
            "#+title: ${citar-citekey}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n")
           :unnarrowed t)
          ("m" "meta" plain "%?"
           :if-new
           (file+head "meta/%<%Y%m%d>--${slug}__%(hiddyn/select-tag).org"
                      "#+title: ${title}\n#+filetags: %(hiddyn/filetags)\n#+CREATED: %U\n#+LAST_MODIFIED: %U")
           :immediate-finish t
           :unnarrowed t)
          ("w" "work" plain "%?"
           :if-new
           (file+head "work/${slug}.org" "#+TITLE: ${title}\n#+FILETAGS: %^g :article:\n")
           :immediate-finish t
           :unnarrowed t)))

  ;; convert title to slug
  (cl-defmethod org-roam-node-slug ((node org-roam-node))
    "Return the slug of NODE."
    (let ((title (org-roam-node-title node))
          (slug-trim-chars '(;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                             768 ; U+0300 COMBINING GRAVE ACCENT
                             769 ; U+0301 COMBINING ACUTE ACCENT
                             770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                             771 ; U+0303 COMBINING TILDE
                             772 ; U+0304 COMBINING MACRON
                             774 ; U+0306 COMBINING BREVE
                             775 ; U+0307 COMBINING DOT ABOVE
                             776 ; U+0308 COMBINING DIAERESIS
                             777 ; U+0309 COMBINING HOOK ABOVE
                             778 ; U+030A COMBINING RING ABOVE
                             780 ; U+030C COMBINING CARON
                             795 ; U+031B COMBINING HORN
                             803 ; U+0323 COMBINING DOT BELOW
                             804 ; U+0324 COMBINING DIAERESIS BELOW
                             805 ; U+0325 COMBINING RING BELOW
                             807 ; U+0327 COMBINING CEDILLA
                             813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                             814 ; U+032E COMBINING BREVE BELOW
                             816 ; U+0330 COMBINING TILDE BELOW
                             817 ; U+0331 COMBINING MACRON BELOW
                             )))
      (cl-flet* ((nonspacing-mark-p (char)
                   (memq char slug-trim-chars))
                 (strip-nonspacing-marks (s)
                   (string-glyph-decompose
                    (apply #'string (seq-remove #'nonspacing-mark-p
                                                (string-glyph-decompose s)))))
                 (cl-replace (title pair)
                   (replace-regexp-in-string (car pair) (cdr pair) title)))
        (let* ((pairs `(
                        ("[^[:alnum:][:digit:][:blank:]]" . "") ;; convert anything not alphanumeric
                        (" " . "-")                   ;; remove sequential underscores
                        ("--*" . "-")                   ;; remove sequential underscores
                        ("^-" . "")                     ;; remove starting underscore
                        ("-$" . "")))                   ;; remove ending underscore
               (slug (-reduce-from #'cl-replace (strip-nonspacing-marks title) pairs)))
          (downcase slug)))))

  (defun my/org-roam-node-find-courses ()
    "Show list of `org-roam-node-find' only under dirA."
    (interactive)
    (org-roam-node-find nil nil
                        (lambda (node)
                          (file-in-directory-p
                           (org-roam-node-file node)
                           (expand-file-name "uni/courses" org-roam-directory)))))
  )

(use-package consult-org-roam
  :commands (consult-org-roam-file-find)
  :custom
  (consult-org-roam-buffer-narrow-key ?r)
  :config
  (consult-org-roam-mode))


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

;; (use-package org-modern
;;   :defer t
;;   :hook
;;   (org-mode . org-modern-mode)
;;   (org-roam-mode . org-modern-mode))

(use-package citar
  :defer t
  :after (org org-roam)
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-org-roam-note-title-template "${author} - ${title}")
  :config

  (setq citar-bibliography "~/GoogleDrive/org/.resources/bibliography.bib")

  (defun citar-file-open (file)
	"Open FILE. Overwritten by hgi, to open pdf files from citar in external PDF viewer and not in internal one."
	(if (or (equal (file-name-extension file) "pdf") (equal (file-name-extension file) "html"))
		(citar-file-open-external (expand-file-name file))
	  (funcall citar-file-open-function (expand-file-name file))))

  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup))

(use-package citar-org-roam
  :after citar
  :custom
  (citar-org-roam-note-title-template "${author} - ${title}")
  (citar-org-roam-capture-template-key "l")
  (citar-org-roam-subdir "uni/papers")
  :config
  (citar-org-roam-mode))

(use-package citar-embark
  :after citar
  :config
  (citar-embark-mode))

(use-package consult-notes
  :after dashboard
  :straight (:type git :host github :repo "mclear-tools/consult-notes")
  :custom

  (consult-notes-file-dir-sources
   '(("course"             ?c "~/GoogleDrive/org/uni/courses/")))

  (consult-notes-org-roam-template
   (concat "${type:20} ${title:70}" (propertize "${fmtime:20}" 'face 'font-lock-comment-face)(propertize "${tags:20}" 'face 'org-tag) "${blinks:3}"))

  :commands (consult-notes
             consult-notes-search-in-all-notes
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
  )

(provide 'orgconfig)

;;; org-config ends here
