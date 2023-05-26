(provide 'core)

(use-package restart-emacs)

(use-package helpful
	:defer t
	)

(use-package evil
	:straight t
	:init
	(setq evil-want-integration t)
	(setq evil-echo-state nil)
	(setq evil-want-keybinding nil)
	(setq evil-want-empty-ex-last-command nil)
	(setq evil-undo-system 'undo-fu)
	(setq evil-search-module 'evil-search)
	:config
	;; (modify-syntax-entry ?_ "w")
	;; (modify-syntax-entry ?- "w")
	(evil-mode 1))

(use-package evil-collection
	:after evil
	:config
	(evil-collection-init))

(use-package evil-commentary
	;; Better Comment Action
	:after evil
	:config
	(evil-define-key 'visual 'global (kbd "gc") 'evil-commentary)
	(evil-define-key 'normal 'global (kbd "gcA") 'comment_end_of_line)
	(evil-define-key 'visual 'global (kbd "gb") 'comment-box)
	(evil-define-key 'normal 'global (kbd "gcc") 'evil-commentary-line))

(defun split-and-follow-horizontally ()
	(interactive)
	(split-window-below)
	(balance-windows)
	(other-window 1))

(defun split-and-follow-vertically ()
	(interactive)
	(split-window-right)
	(balance-windows)
	(other-window 1))

;; Use escape to remove hightlight in normal mode
(evil-define-key 'normal 'global (kbd "<escape>") 'evil-ex-nohighlight)
(evil-define-key 'insert 'global (kbd "C-y") 'evil-paste-after)
(evil-define-key 'normal 'global (kbd "C-u") 'evil-scroll-up)
(evil-set-leader 'normal (kbd "SPC"))
(evil-set-leader 'visual (kbd "SPC"))

;; Offer to diff buffer when closing it
(defun my-kill-this-buffer ()
	(interactive)
	(catch 'quit
		(save-window-excursion
			(let (done)
				(when (and buffer-file-name (buffer-modified-p))
					(while (not done)
						(let ((response (read-char-choice
														 (format "Save file %s? (y, n, d, q) " (buffer-file-name))
														 '(?y ?n ?d ?q))))
							(setq done (cond
													((eq response ?q) (throw 'quit nil))
													((eq response ?y) (save-buffer) t)
													((eq response ?n) (set-buffer-modified-p nil) t)
													((eq response ?d) (diff-buffer-with-file) nil))))))
				(kill-buffer (current-buffer))))))

(defun smart-for-files ()
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-find-file)
    (call-interactively #'helm-find-files)))

(defun smart-for-buffer ()
  (interactive)
  (if (projectile-project-p)
      (helm-projectile-switch-to-buffer)
    (helm-buffers-list)))

;;Settings normal global keybindings
(evil-define-key 'normal 'global
	(kbd ";") 'evil-ex
	;; (kbd ":") 'execute-extended-command
	(kbd ":") 'helm-M-x
	(kbd "<leader>ff") 'smart-for-files
	(kbd "<leader>fw") 'find-file-other-window
	(kbd "<leader>fr") 'consult-recent-file
	(kbd "<leader>fg") 'helm-do-grep-ag
	(kbd "<leader>dj") 'dired-jump
	(kbd "<leader>dd") 'dired
	(kbd "<leader>bb") 'smart-for-buffer
	(kbd "<leader>bw") 'consult-buffer-other-window
	(kbd "<leader>w") 'save-buffer
	(kbd "<leader> q b") 'kill-buffer
	(kbd "Q") 'my-kill-this-buffer
	(kbd "C-s v") 'split-and-follow-vertically
	(kbd "C-s h") 'split-and-follow-horizontally
	(kbd "<leader>gt") 'google-this
	(kbd "<leader>gh") 'dashboard-open
	(kbd "<leader>ee") 'eval-buffer
	(kbd "<leader>es") 'eval-expression
	(kbd "<leader>er") 'eval-region
	(kbd "<leader>ef") 'eval-defun
	(kbd "<leader>pp") 'helm-projectile-switch-project
	(kbd "<leader>cc") 'calc
	(kbd "<leader> q c") 'quick-calc
	(kbd "<leader>h") 'helm-mini
	(kbd "S") 'evil-surround-edit
	(kbd ",r") 'evil-surround-delete
	(kbd ",c") 'evil-surround-change
	)

(evil-define-key 'insert 'global (kbd "C-<backspace>") 'evil-delete-backward-word)
(evil-define-key 'visual 'global (kbd "<leader>gg") 'google-this-noconfirm)
(evil-define-key 'normal 'prog-mode-map (kbd "<leader>m") 'rainbow-mode)

(use-package evil-goggles
	:straight t
	:after evil
	:config
	(evil-goggles-mode)
	(setq evil-goggle-duration 0.5)
	(evil-goggles-use-diff-faces))

(use-package consult)

(use-package undo-fu
	:straight t)

(use-package evil-surround
	:after evil
	:straight t
	:config
	(global-evil-surround-mode 1))

(defun comment_end_of_line ()
	(interactive)
	(call-interactively 'comment-dwim)
	(call-interactively 'evil-append))

(define-key evil-normal-state-map (kbd "Y") (kbd "y$"))

(use-package dired
	:commands dired
	:straight nil
	:ensure nil
	:config
	(evil-define-key 'normal dired-mode-map
		(kbd "f") 'dired-narrow-fuzzy
		(kbd "T") 'dired-create-empty-file
		(kbd "<RET>") 'dired-find-alternate-file
		(kbd "<escape>") 'keyboard-escape-quit
		(kbd "u") 'dired-up-directory))

(use-package dired-narrow
	:config
	(defun dired-narrow-ex-ac ()
		;; Revert buffer and enter the directory after narrowing
		(revert-buffer)
		(dired-find-alternate-file))
	(setq dired-narrow-exit-when-1-left t)
	(setq dired-narrow-exit-action 'dired-narrow-ex-ac)
	)

(use-package which-key
	:defer t
	:straight t
	:init
	(which-key-setup-minibuffer)
	(which-key-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
	:init
	(savehist-mode))

(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

(use-package marginalia
	:config
	(marginalia-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
	:straight t
	:init
	(savehist-mode))

;; A few more useful configurations...
(use-package emacs
	:init
	;; Add prompt indicator to `completing-read-multiple'.
	;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
	(defun crm-indicator (args)
		(cons (format "[CRM%s] %s"
									(replace-regexp-in-string
									 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
									 crm-separator)
									(car args))
					(cdr args)))
	(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

	;; Do not allow the cursor in the minibuffer prompt
	(setq minibuffer-prompt-properties
				'(read-only t cursor-intangible t face minibuffer-prompt))
	(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

	;; Enable recursive minibuffers
	(setq enable-recursive-minibuffers t))

(use-package doom-modeline
	:straight t
	:config
	(setq doom-modeline-buffer-encoding nil)
	(setq doom-modeline-height 40)
	:init (doom-modeline-mode 1))

(use-package lsp-mode
	:straight t
	:init
	(add-hook 'LaTeX-mode-hook 'lsp)
	:config
	(setq lsp-tex-server 'texlab)
	(setq lsp-enable-symbol-highlighting nil)
	(setq lsp-lens-enable nil)
	;; (setq lsp-headerline-breadcrumb-enable nil)
	(setq lsp-keymap-prefix "C-c l")
	:hook
	((lua-mode . lsp)
	 (lsp-mode . lsp-enable-which-key-integration)) ;whichkey-integration
	:commands lsp)

(use-package lua-mode
	:defer t
	:straight t)

;; optionally
(use-package lsp-ui
	:defer t
	:straight t
	:commands lsp-ui-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company-math)

(use-package company-jedi)

(use-package company
  :diminish company-mode
	:hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
	:bind
  (:map company-active-map
        ([tab] . smarter-tab-to-complete)
        ("C-d" . company-box-doc-manually)
        ("TAB" . smarter-tab-to-complete))
	:custom
	(setq company-tooltip-offset-display 'lines	;; Show number before and after current candidates
				company-tooltip-flip-when-above t	;; Avoid screen breaking when at the bottom of the buffer
				company-tooltip-minimum 2
				company-minimum-prefix-length 2
				company-tooltip-align-annotations t
				;; company-require-match 'never
				company-tooltip-limit 5
				company-idle-delay 0.0)
  (company-global-modes '(not shell-mode eaf-mode))
	:init
	(setq company-backends '((company-capf :separate company-files company-yasnippet)))
	:config
	(add-hook 'python-mode-hook
						(lambda ()
							(set (make-local-variable 'company-backends) '(company-capf company-jedi company-files))))
	(add-hook 'LaTeX-mode-hook
						(lambda ()
							(set (make-local-variable 'company-backends) '(company-capf company-math company-files))))
	(global-company-mode 1)
	(defun smarter-tab-to-complete ()
    "Try to `org-cycle', `yas-expand', and `yas-next-field' at current cursor position.
     If all failed, try to complete the common part with `company-complete-common'"
    (interactive)
    (when yas-minor-mode
      (let ((old-point (point))
            (old-tick (buffer-chars-modified-tick))
            (func-list
             (if (equal major-mode 'org-mode) '(org-cycle yas-expand yas-next-field tab-jump-out)
               '(yas-expand yas-next-field tab-jump-out))))
        (catch 'func-suceed
          (dolist (func func-list)
            (ignore-errors (call-interactively func))
            (unless (and (eq old-point (point))
                         (eq old-tick (buffer-chars-modified-tick)))
              (throw 'func-suceed t)))
          (company-complete-common)))))
	)

(use-package company-box
  :diminish
  :if (display-graphic-p)
  :defines company-box-icons-all-the-icons
  :hook (company-mode . company-box-mode)
  :custom
	(company-box-doc-enable nil)
	(company-box-scrollbar nil)
  (company-box-backends-colors nil)
  (company-box-doc-delay 0.1)
  (company-box-doc-frame-parameters '((internal-border-width . 1)
                                      (left-fringe . 3)
                                      (right-fringe . 3)))
  :config
  (with-no-warnings
    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (or (derived-mode-p 'emacs-lisp-mode) (derived-mode-p 'lisp-mode))
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

    ;; Credits to Centaur for these configurations
    ;; Display borders and optimize performance
    (defun my-company-box--display (string on-update)
      "Display the completions."
      (company-box--render-buffer string on-update)

      (let ((frame (company-box--get-frame))
            (border-color (face-foreground 'font-lock-comment-face nil t)))
        (unless frame
          (setq frame (company-box--make-frame))
          (company-box--set-frame frame))
        (company-box--compute-frame-position frame)
        (company-box--move-selection t)
        (company-box--update-frame-position frame)
        (unless (frame-visible-p frame)
          (make-frame-visible frame))
        (company-box--update-scrollbar frame t)
        (set-face-background 'internal-border border-color frame)
        (when (facep 'child-frame-border)
          (set-face-background 'child-frame-border border-color frame)))
      (with-current-buffer (company-box--get-buffer)
        (company-box--maybe-move-number (or company-box--last-start 1))))
    (advice-add #'company-box--display :override #'my-company-box--display)

    (defun my-company-box-doc--make-buffer (object)
      (let* ((buffer-list-update-hook nil)
             (inhibit-modification-hooks t)
             (string (cond ((stringp object) object)
                           ((bufferp object) (with-current-buffer object (buffer-string))))))
        (when (and string (> (length (string-trim string)) 0))
          (with-current-buffer (company-box--get-buffer "doc")
            (erase-buffer)
            (insert (propertize "\n" 'face '(:height 0.5)))
            (insert string)
            (insert (propertize "\n\n" 'face '(:height 0.5)))

            ;; Handle hr lines of markdown
            ;; @see `lsp-ui-doc--handle-hr-lines'
            (with-current-buffer (company-box--get-buffer "doc")
              (let (bolp next before after)
                (goto-char 1)
                (while (setq next (next-single-property-change (or next 1) 'markdown-hr))
                  (when (get-text-property next 'markdown-hr)
                    (goto-char next)
                    (setq bolp (bolp)
                          before (char-before))
                    (delete-region (point) (save-excursion (forward-visible-line 1) (point)))
                    (setq after (char-after (1+ (point))))
                    (insert
                     (concat
                      (and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
                      (propertize "\n" 'face '(:height 0.5))
                      (propertize " "
                                  'display '(space :height (1))
                                  'company-box-doc--replace-hr t
                                  'face `(:background ,(face-foreground 'font-lock-comment-face)))
                      (propertize " " 'display '(space :height (1)))
                      (and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))

            (setq mode-line-format nil
                  display-line-numbers nil
                  header-line-format nil
                  show-trailing-whitespace nil
                  cursor-in-non-selected-windows nil)
            (current-buffer)))))
    (advice-add #'company-box-doc--make-buffer :override #'my-company-box-doc--make-buffer)

    ;; Display the border and fix the markdown header properties
    (defun my-company-box-doc--show (selection frame)
      (cl-letf (((symbol-function 'completing-read) #'company-box-completing-read)
                (window-configuration-change-hook nil)
                (inhibit-redisplay t)
                (display-buffer-alist nil)
                (buffer-list-update-hook nil))
        (-when-let* ((valid-state (and (eq (selected-frame) frame)
                                       company-box--bottom
                                       company-selection
                                       (company-box--get-frame)
                                       (frame-visible-p (company-box--get-frame))))
                     (candidate (nth selection company-candidates))
                     (doc (or (company-call-backend 'quickhelp-string candidate)
                              (company-box-doc--fetch-doc-buffer candidate)))
                     (doc (company-box-doc--make-buffer doc)))
          (let ((frame (frame-local-getq company-box-doc-frame))
                (border-color (face-foreground 'font-lock-comment-face nil t)))
            (unless (frame-live-p frame)
              (setq frame (company-box-doc--make-frame doc))
              (frame-local-setq company-box-doc-frame frame))
            (set-face-background 'internal-border border-color frame)
            (when (facep 'child-frame-border)
              (set-face-background 'child-frame-border border-color frame))
            (company-box-doc--set-frame-position frame)

            ;; Fix hr props. @see `lsp-ui-doc--fix-hr-props'
            (with-current-buffer (company-box--get-buffer "doc")
              (let (next)
                (while (setq next (next-single-property-change (or next 1) 'company-box-doc--replace-hr))
                  (when (get-text-property next 'company-box-doc--replace-hr)
                    (put-text-property next (1+ next) 'display
                                       '(space :align-to (- right-fringe 1) :height (1)))
                    (put-text-property (1+ next) (+ next 2) 'display
                                       '(space :align-to right-fringe :height (1)))))))

            (unless (frame-visible-p frame)
              (make-frame-visible frame))))))
    (advice-add #'company-box-doc--show :override #'my-company-box-doc--show)

    (defun my-company-box-doc--set-frame-position (frame)
      (-let* ((frame-resize-pixelwise t)

              (box-frame (company-box--get-frame))
              (box-position (frame-position box-frame))
              (box-width (frame-pixel-width box-frame))
              (box-height (frame-pixel-height box-frame))
              (box-border-width (frame-border-width box-frame))

              (window (frame-root-window frame))
              ((text-width . text-height) (window-text-pixel-size window nil nil
                                                                  (/ (frame-pixel-width) 2)
                                                                  (/ (frame-pixel-height) 2)))
              (border-width (or (alist-get 'internal-border-width company-box-doc-frame-parameters) 0))

              (x (- (+ (car box-position) box-width) border-width))
              (space-right (- (frame-pixel-width) x))
              (space-left (car box-position))
              (fringe-left (or (alist-get 'left-fringe company-box-doc-frame-parameters) 0))
              (fringe-right (or (alist-get 'right-fringe company-box-doc-frame-parameters) 0))
              (width (+ text-width border-width fringe-left fringe-right))
              (x (if (> width space-right)
                     (if (> space-left width)
                         (- space-left width)
                       space-left)
                   x))
              (y (cdr box-position))
              (bottom (+ company-box--bottom (frame-border-width)))
              (height (+ text-height (* 2 border-width)))
              (y (cond ((= x space-left)
                        (if (> (+ y box-height height) bottom)
                            (+ (- y height) border-width)
                          (- (+ y box-height) border-width)))
                       ((> (+ y height) bottom)
                        (- (+ y box-height) height))
                       (t y))))
        (set-frame-position frame (max x 0) (max y 0))
        (set-frame-size frame text-width text-height t)))
    (advice-add #'company-box-doc--set-frame-position :override #'my-company-box-doc--set-frame-position))

  (when (require 'all-the-icons nil t)
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (declare-function all-the-icons-octicon 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 1.0 :v-adjust -0.2))
            (Text . ,(all-the-icons-faicon "text-width" :height 1.0 :v-adjust -0.02))
            (Method . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 1.0 :v-adjust -0.02))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 1.0 :v-adjust -0.2))
            (Value . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 1.0 :v-adjust -0.2))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 1.0 :v-adjust -0.2))
            (Color . ,(all-the-icons-material "palette" :height 1.0 :v-adjust -0.2))
            (File . ,(all-the-icons-faicon "file-o" :height 1.0 :v-adjust -0.02))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 1.0 :v-adjust -0.2))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 1.0 :v-adjust -0.02))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2))
            (Constant . ,(all-the-icons-faicon "square-o" :height 1.0 :v-adjust -0.1))
            (Struct . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-octicon "zap" :height 1.0 :v-adjust 0 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 1.0 :v-adjust -0.2))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 1.0 :v-adjust -0.02))
            (Template . ,(all-the-icons-material "format_align_left" :height 1.0 :v-adjust -0.2)))
          company-box-icons-alist 'company-box-icons-all-the-icons)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Editorconfig, auto set indenting
(use-package editorconfig
	:config
	(editorconfig-mode 1))

;; Autopair
(electric-pair-mode 1)

;; Highlight nested parentheses
(use-package rainbow-delimiters
	:config
	(add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Highlight colorstring with the right color
(use-package rainbow-mode
	:config
	(add-hook 'prog-mode #'rainbow-mode))

(use-package dashboard
	:ensure t
	:config
	(setq dashboard-banner-logo-title "Welcome Back Goblin")
	;; Content is not centered by default. To center, set
	(setq dashboard-startup-banner "~/.config/emacs/themes/logo.txt")
	(setq dashboard-center-content t)
	(setq dashboard-set-heading-icons t)
	(setq dashboard-set-file-icons t)
	(setq dashboard-set-navigator t)
	(setq dashboard-items '((recents  . 10)))
	;; (bookmarks . 5))
	;; (projects . 5)))
	;; (agenda . 5)
	;; (registers . 5)))
	(dashboard-setup-startup-hook))

(use-package google-this
	:defer t)

(use-package org-bullets
	:defer t
	:config
	;; use org-bullets-mode for utf8 symbols as org bullets
	(require 'org-bullets)
	;; make available "org-bullet-face" such that I can control the font size individually
	(setq org-bullets-face-name (quote org-bullet-face))
	(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
	(setq org-bullets-bullet-list '("✡" "⎈" "✽" "✲" "✱" "✻" "✼" "✽" "✾" "✿" "❀" "❁" "❂" "❃" "❄" "❅" "❆" "❇"))
	(setq org-hide-emphasis-markers t)
	(custom-set-faces
	 '(org-level-1 ((t (:inherit outline-1 :height 1.4))))
	 '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
	 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
	 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
	 '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
	(setq org-agenda-files '("~/org"))
	:init
	;; (add-hook 'org-mode-hook 'visual-line-mode)
	(add-hook 'org-mode-hook 'org-indent-mode)
	(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package tab-jump-out
	:custom
	(tab-jump-out-mode 1))

(straight-use-package '(targets :type git :host github
																:repo "dvzubarev/targets.el"
																:branch "fix-remote"))

(use-package targets
	:config
	(setq targets-text-objects nil)
	(targets-setup nil)
	(targets-define-composite-to any-block
		(("(" ")" pair)
		 ("[" "]" pair)
		 ("{" "}" pair)
		 ;; ("<" ">" pair)
		 )
		:bind t
		:next-key "N"
		:last-key "L"
		:around-key nil
		:inside-key nil
		:keys "b")
	(targets-define-composite-to any-quote
		(("\"" "\"" quote)
		 ("'" "'" quote))
		:bind t
		:next-key "N"
		:last-key "L"
		:around-key nil
		:inside-key nil
		:keys "q")
	(targets-define-to word 'evil-word nil object :bind t :keys "w")
	(targets-define-to double-quote
										 "\"" nil quote
										 :bind t
										 :next-key "N"
										 :last-key "L"
										 :around-key nil
										 :inside-key nil
										 :keys "q"
										 :hooks (emacs-lisp-mode-hook)))

(use-package nyan-mode)

(use-package org-download
	:after org-mode
	:hook
	(add-hook 'dired-mode-hook 'org-download-enable))

(defun export-buffer-to-pdf ()
	"Export current buffer to PDF using Pandoc asynchronously without minibuffer output."
	(interactive)
	(let* ((output-file (concat (file-name-sans-extension (buffer-file-name)) ".pdf"))
				 (process-buffer (generate-new-buffer "*pandoc-export*"))
				 (exit-code (call-process "pandoc" nil process-buffer nil
																	(buffer-file-name) "-o" output-file "-V" "geometry:margin=10mm" "--template=template.latex" "--pdf-engine=xelatex")))
		(if (zerop exit-code)
				(message "Exported to %s" output-file)
			(with-current-buffer process-buffer
				(message "Export failed: %s" (buffer-string))))
		(kill-buffer process-buffer)))

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

;; Auto reload pdf and suppress messages
(global-auto-revert-mode 1)
(setq auto-revert-verbose nil)

(use-package pdf-tools
	:config
	(setq-default pdf-view-display-size 'fit-page) ; Fit page width
	(setq pdf-annot-activate-created-annotations t) ; Enable annotations
	(define-key pdf-view-mode-map (kbd "j") 'pdf-view-next-line-or-next-page)
	(define-key pdf-view-mode-map (kbd "k") 'pdf-view-previous-line-or-previous-page)
	(define-key pdf-view-mode-map (kbd "h") 'image-backward-hscroll)
	(define-key pdf-view-mode-map (kbd "l") 'image-forward-hscroll)
	(define-key pdf-view-mode-map (kbd "g") 'pdf-view-first-page)
	(define-key pdf-view-mode-map (kbd "G") 'pdf-view-last-page)
	(define-key pdf-view-mode-map (kbd "C-j") 'pdf-view-next-page)
	(define-key pdf-view-mode-map (kbd "C-k") 'pdf-view-previous-page)
	(define-key pdf-view-mode-map (kbd "C-d") 'pdf-view-scroll-up-or-next-page)
	(define-key pdf-view-mode-map (kbd "C-u") 'pdf-view-scroll-down-or-previous-page)
	)

(use-package markdown-mode
	:config
	(evil-define-key 'normal markdown-mode-map
		(kbd "<leader>ee") 'export-buffer-to-pdf
		(kbd "<leader>ez") 'open-pdf-with-zathura
		(kbd "<leader>ep") 'open-pdf-with-pdf-tools)
	(setq nuke-trailing-whitespace-p nil)
	:mode ("README\\.md\\'" . gfm-mode)
	:init
	(setq markdown-enable-math t))
;; :init (setq markdown-command "multimarkdown"))

;;;;;;;;;;;
;; Latex ;;
;;;;;;;;;;;

(use-package tex
	:straight auctex
	:config
	(setq TeX-save-query nil)
	(setq TeX-clean-confirm nil)
	(setq TeX-source-correlate-method 'synctex)
	(TeX-source-correlate-mode 1)
	(setq TeX-source-correlate-start-server t)
	(add-to-list 'TeX-view-program-selection
							 '(output-pdf "Zathura"))
	:init
	(add-hook 'LaTeX-mode-hook 'prettify-symbols-mode))

(evil-define-key 'normal LaTeX-mode-map
	(kbd "<leader>tm") 'reftex-toc
	(kbd "<leader>tt") 'lsp-ui-imenu
	)

(use-package yasnippet
	:defer t
	:init
	(yas-minor-mode))

(use-package aas
	:hook
	(org-mode . aas-activate-for-major-mode)
	(markdown-mode . aas-activate-for-major-mode)
	(LaTeX-mode . aas-activate-for-major-mode)
	:config
	(aas-set-snippets 'latex-mode
		"mk" (lambda () (interactive)
					 (yas-expand-snippet "\\\\($1\\\\) $0"))
		"dm" (lambda () (interactive)
					 (yas-expand-snippet "\\[ \n $1 \n \\] \n \n $0")))
	(aas-set-snippets 'org-mode
		"mk" (lambda () (interactive)
					 (yas-expand-snippet "\\\\( $1 \\\\) $0"))
		"dm" (lambda () (interactive)
					 (yas-expand-snippet "\\[ \n $1 \n \\] \n \n $0")))
	(aas-set-snippets 'markdown-mode
		"mk" (lambda () (interactive)
					 (yas-expand-snippet "$$1$ $0"))
		"dm" (lambda () (interactive)
					 (yas-expand-snippet "$$ \n $1 \n $$ \n \n $0"))))

(use-package laas
	:straight (laas :type git :host github :repo "Stefanomarton/LaTeX-auto-activating-snippets")
	:hook (LaTeX-mode . laas-mode)
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

(use-package latex-table-wizard
	:after tex)

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
										))

;;;;;;;;;;;;;;;
;; AvyConfig ;;
;;;;;;;;;;;;;;;

(use-package avy
	:straight t
	:config
	(setq avy-timeout-seconds 0.2)
	(setq avy-keys (nconc (number-sequence ?a ?z)))
	)

;; (evil-define-key 'operator 'global (kbd "l") 'avy-goto-line)

(evil-define-key 'normal 'global (kbd "s") 'evil-avy-goto-char-timer)
(evil-define-key 'motion 'global (kbd "s") 'evil-avy-goto-char-timer)
(evil-define-key 'operator 'global (kbd "s") 'evil-avy-goto-char-timer)
(evil-define-key 'normal 'global (kbd "f") 'evil-avy-goto-char-in-line)
(evil-define-key 'motion 'global (kbd "f") 'evil-avy-goto-char-in-line)
(evil-define-key 'operator 'global (kbd "f") 'evil-avy-goto-char-in-line)
(evil-define-key 'operator 'global (kbd "R") 'avy-copy-region)
;; (evil-define-key 'motion 'global (kbd "L") 'avy-copy-line)

(setq avy-timeout-seconds 0.25)

(use-package expand-region
	:config
	(defun expand-region ()
		"Repeat the `er/expand-region' command."
		(interactive)
		(dotimes (_ 2)
			(call-interactively 'er/expand-region)))
	(evil-define-key 'normal 'global (kbd "C-SPC") 'expand-region)
	(setq expand-region-subword-enabled t)
	)

(use-package org-fragtog
	:after org-mode
	:config
	;; (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.6))
	(plist-put org-format-latex-options :justify 'center)
	)
;; Daemon mode configs

(pcase system-type
	('gnu/linux "It's Linux!")
	('windows-nt "It's Windows!")
	('darwin "It's macOS!"))

(if (daemonp)
		(message "Loading in the daemon!")
	(message "Loading in regular Emacs!"))

(defun efs/set-font-faces ()
	(message "Setting faces!")
	(set-face-attribute 'default nil :font "JetBrainsMono Nerd Font" :height 150)

	;; Set the fixed pitch face
	(set-face-attribute 'fixed-pitch nil :font "JetBrainsMono Nerd Font" :height 150)

	;; Set the variable pitch face
	(set-face-attribute 'variable-pitch nil :font "JetBrainsMono Nerd Font" :height 150 :weight 'regular))

(if (daemonp)
		(add-hook 'after-make-frame-functions
							(lambda (frame)
								;; (setq doom-modeline-icon t)
								(with-selected-frame frame
									(efs/set-font-faces))))
	(efs/set-font-faces)
	(setq highlight-indent-guides-method 'character)
	(setq doom-modeline-icon t)
	)
(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Terminal specific configuration

(if (display-graphic-p)
		;; GUI mode
		(progn
			(nyan-mode 1))
	;; Terminal mode
	())

(use-package org-modern
	:after org-mode
	:hook
	(add-hook 'org-mode-hook #'org-modern-mode))

(use-package gptel)

(use-package format-all
	:init
	(add-hook 'format-all-mode-hook 'format-all-ensure-formatter)
	(add-hook 'prog-mode-hook 'format-all-mode)
	(add-hook 'LaTeX-mode-hook 'format-all-mode)
	)

(use-package yasnippet
	:commands (yas-minor-mode) ; autoload `yasnippet' when `yas-minor-mode' is called
																				; using any means: via a hook or by user
																				; Feel free to add more commands to this
																				; list to suit your needs.
	:hook
	(prog-mode . yas-minor-mode)
	(laas-mode . yas-minor-mode)
	:config ; stuff to do after requiring the package
	(setq yas-snippet-dirs '("~/.config/emacs/snippets"))
	(progn
		(yas-reload-all)))

(use-package projectile
	:custom
	(setq projectile-enable-caching t)
	(setq projectile-track-known-projects-automatically nil)
	(setq projectile-completion-system 'helm)
	:config
	(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)
	:init
	(setq projectile-known-projects-file "~/.config/emacs/project.el")
	(setq projectile-indexing-method 'native)
	(projectile-mode))

(use-package helm
	:init
	(helm-mode 1)
	:bind
	(("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 :map helm-map
	 ("C-j" . helm-next-line)
	 ("C-k" . helm-previous-line)
	 ("<escape>" . helm-keyboard-quit)
	 ("<tab>" . helm-execute-persistent-action)
	 ("C-i" . helm-execute-persistent-action)
	 ("C-<backspace>" . helm-find-files-up-one-level)
	 ("C-z"  . helm-select-action))
	:config
	(evil-define-key 'global 'helm-map (kbd "<escape>") 'keyboard-escape-quit)
	(global-set-key (kbd "C-c h") 'helm-command-prefix)
	(setq helm-split-window-in-side-p           t
				helm-move-to-line-cycle-in-source     nil
				helm-ff-search-library-in-sexp        t
				helm-scroll-amount                    8
				helm-ff-file-name-history-use-recentf t
				helm-echo-input-in-header-line t
				helm-autoresize-max-height 40
				helm-autoresize-min-height 40
				helm-M-x-fuzzy-math t
				helm-buffers-fuzzy-matching t
				helm-recentf-fuzzy-match    t
				helm-mode-fuzzy-match    t
				helm-follow-mode-persistent t)
	(helm-autoresize-mode 1)
	)

(with-eval-after-load 'helm-files
	(dolist (keymap (list helm-find-files-map helm-read-file-map))
		(define-key keymap (kbd "C-l") 'helm-execute-persistent-action)
		(define-key keymap (kbd "C-h") 'helm-find-files-up-one-level)
		(define-key keymap (kbd "C-d") 'helm-ff-run-delete-file)
		;; rebind `describe-key' for convenience
		(define-key keymap (kbd "C-S-h") 'describe-key)))

(with-eval-after-load 'helm-buffers
	(dolist (keymap (list helm-buffer-map))
		(define-key keymap (kbd "C-d") 'helm-buffer-run-kill-buffers)))

(use-package rg)

(use-package helm-rg)

(use-package helm-c-yasnippet
	:after yasnippet)

(use-package helm-projectile)

(use-package magit)
;; prepare the arguments
(setq dotfiles-git-dir (concat "--git-dir=" (expand-file-name "~/.dotfiles")))
(setq dotfiles-work-tree (concat "--work-tree=" (expand-file-name "~")))

;; function to start magit on dotfiles
(defun dotfiles-magit-status ()
	(interactive)
	(add-to-list 'magit-git-global-arguments dotfiles-git-dir)
	(add-to-list 'magit-git-global-arguments dotfiles-work-tree)
	(call-interactively 'magit-status))
(global-set-key (kbd "<leader> gd") 'dotfiles-magit-status)

;; wrapper to remove additional args before starting magit
(defun magit-status-with-removed-dotfiles-args ()
	(interactive)
	(setq magit-git-global-arguments (remove dotfiles-git-dir magit-git-global-arguments))
	(setq magit-git-global-arguments (remove dotfiles-work-tree magit-git-global-arguments))
	(call-interactively 'magit-status))
;; redirect global magit hotkey to our wrapper
(global-set-key (kbd "<leader> gg") 'magit-status-with-removed-dotfiles-args)
(define-key magit-mode-map (kbd "<leader> gg") 'magit-status-with-removed-dotfiles-args)

(use-package vterm
	:commands vterm
	:config
	(setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"
				vterm-max-scrollback 10000
				vterm-shell "zsh"
				))

(use-package python-mode
	:defer t
	:config
	(autoload 'python-mode "python-mode" "Python Mode." t)
	(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
	(add-to-list 'interpreter-mode-alist '("python" . python-mode))
	:custom
  (python-indent-offset 4)
	(setq python-shell-interpreter "ipython"
				python-shell-interpreter-args "-i --simple-prompt"))

(use-package lsp-pyright
  :hook (python-mode . (lambda () (require 'lsp-pyright)))
  :custom
  (LSP-PYRight-multi-root nil))
