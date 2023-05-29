(use-package company-jedi)

(use-package company
	:diminish company-mode
	:hook ((prog-mode LaTeX-mode latex-mode ess-r-mode) . company-mode)
	:bind
	(:map company-active-map
				([tab] . smarter-tab-to-complete)
				("C-d" . company-box-doc-manually)
				("tab" . smarter-tab-to-complete))
	:custom
	(setq company-tooltip-offset-display 'lines	;; Show number before and after current candidates
				company-tooltip-flip-when-above t	;; Avoid screen breaking when at the bottom of the buffer
				company-tooltip-minimum 2
				company-minimum-prefix-length 2
				company-tooltip-align-annotations t
				company-require-match 'never
				company-tooltip-limit 5
				company-auto-complete t
				company-idle-delay 0.0)
	(company-global-modes '(not shell-mode eaf-mode))
	:init
	(setq company-backends '((company-capf :with company-yasnippet company-files)))
	:config
	(add-hook 'python-mode-hook
						(lambda ()
							(set (make-local-variable 'company-backends) '(company-capf :with company-jedi company-files))))
	(add-hook 'markdown-mode-hook
						(lambda ()
							(set (make-local-variable 'company-backends) '(:separate company-capf company-yasnippet company-files))))
	(add-hook 'emacs-lisp-mode-hook
						(lambda ()
							(set (make-local-variable 'company-backends) '(company-elisp :with ( company-yasnippet company-files )))))
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

	;; (custom-set-faces
	;;  '(company-tooltip
	;; 	 ((t (:background "#191C25" :foreground "#ECEFF4"))))
	;;  '(company-tooltip-selection
	;; 	 ((t (:background "#81A1C1" :foreground "#ECEFF4"))))
	;;  '(company-tooltip-common ((t (:weight bold :foreground "pink1"))))
	;;  '(company-scrollbar-fg ((t (:background "ivory3"))))
	;;  '(company-scrollbar-bg ((t (:background "ivory2"))))
	;;  '(company-tooltip-annotation ((t (:foreground "MistyRose2")))))
	)

(use-package company-box
	:diminish
	:if (display-graphic-p)
	:defines company-box-icons-all-the-icons
	:hook (company-mode . company-box-mode)
	:custom
	(company-box-doc-enable nil)
	(company-box-scrollbar nil)
	(company-box-doc-delay 0.1)
	(company-box-doc-frame-parameters '((internal-border-width . 1)
																			(left-fringe . 3)
																			(right-fringe . 3)))
	:config
	;; Fix company-yasnippet no sense colors
	(setq company-box-backends-colors
				'((company-yasnippet :all "#191C25" :selected
														 (:background "#191C25" :foreground "#ECEFF4"))))
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
		(advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)))

;; 		;; Credits to Centaur for these configurations
;; 		;; Display borders and optimize performance
;; 		(defun my-company-box--display (string on-update)
;; 			"Display the completions."
;; 			(company-box--render-buffer string on-update)

;; 			(let ((frame (company-box--get-frame))
;; 						(border-color (face-foreground 'font-lock-comment-face nil t)))
;; 				(unless frame
;; 					(setq frame (company-box--make-frame))
;; 					(company-box--set-frame frame))
;; 				(company-box--compute-frame-position frame)
;; 				(company-box--move-selection t)
;; 				(company-box--update-frame-position frame)
;; 				(unless (frame-visible-p frame)
;; 					(make-frame-visible frame))
;; 				(company-box--update-scrollbar frame t)
;; 				(set-face-background 'internal-border border-color frame)
;; 				(when (facep 'child-frame-border)
;; 					(set-face-background 'child-frame-border border-color frame)))
;; 			(with-current-buffer (company-box--get-buffer)
;; 				(company-box--maybe-move-number (or company-box--last-start 1))))
;; 		(advice-add #'company-box--display :override #'my-company-box--display)

;; 		(defun my-company-box-doc--make-buffer (object)
;; 			(let* ((buffer-list-update-hook nil)
;; 						 (inhibit-modification-hooks t)
;; 						 (string (cond ((stringp object) object)
;; 													 ((bufferp object) (with-current-buffer object (buffer-string))))))
;; 				(when (and string (> (length (string-trim string)) 0))
;; 					(with-current-buffer (company-box--get-buffer "doc")
;; 						(erase-buffer)
;; 						(insert (propertize "\n" 'face '(:height 0.5)))
;; 						(insert string)
;; 						(insert (propertize "\n\n" 'face '(:height 0.5)))

;; 						;; Handle hr lines of markdown
;; 						;; @see `lsp-ui-doc--handle-hr-lines'
;; 						(with-current-buffer (company-box--get-buffer "doc")
;; 							(let (bolp next before after)
;; 								(goto-char 1)
;; 								(while (setq next (next-single-property-change (or next 1) 'markdown-hr))
;; 									(when (get-text-property next 'markdown-hr)
;; 										(goto-char next)
;; 										(setq bolp (bolp)
;; 													before (char-before))
;; 										(delete-region (point) (save-excursion (forward-visible-line 1) (point)))
;; 										(setq after (char-after (1+ (point))))
;; 										(insert
;; 										 (concat
;; 											(and bolp (not (equal before ?\n)) (propertize "\n" 'face '(:height 0.5)))
;; 											(propertize "\n" 'face '(:height 0.5))
;; 											(propertize " "
;; 																	'display '(space :height (1))
;; 																	'company-box-doc--replace-hr t
;; 																	'face `(:background ,(face-foreground 'font-lock-comment-face)))
;; 											(propertize " " 'display '(space :height (1)))
;; 											(and (not (equal after ?\n)) (propertize " \n" 'face '(:height 0.5)))))))))

;; 						(setq mode-line-format nil
;; 									display-line-numbers nil
;; 									header-line-format nil
;; 									show-trailing-whitespace nil
;; 									cursor-in-non-selected-windows nil)
;; 						(current-buffer)))))
;; 		(advice-add #'company-box-doc--make-buffer :override #'my-company-box-doc--make-buffer)

;; 		;; Display the border and fix the markdown header properties
;; 		(defun my-company-box-doc--show (selection frame)
;; 			(cl-letf (((symbol-function 'completing-read) #'company-box-completing-read)
;; 								(window-configuration-change-hook nil)
;; 								(inhibit-redisplay t)
;; 								(display-buffer-alist nil)
;; 								(buffer-list-update-hook nil))
;; 				(-when-let* ((valid-state (and (eq (selected-frame) frame)
;; 																			 company-box--bottom
;; 																			 company-selection
;; 																			 (company-box--get-frame)
;; 																			 (frame-visible-p (company-box--get-frame))))
;; 										 (candidate (nth selection company-candidates))
;; 										 (doc (or (company-call-backend 'quickhelp-string candidate)
;; 															(company-box-doc--fetch-doc-buffer candidate)))
;; 										 (doc (company-box-doc--make-buffer doc)))
;; 					(let ((frame (frame-local-getq company-box-doc-frame))
;; 								(border-color (face-foreground 'font-lock-comment-face nil t)))
;; 						(unless (frame-live-p frame)
;; 							(setq frame (company-box-doc--make-frame doc))
;; 							(frame-local-setq company-box-doc-frame frame))
;; 						(set-face-background 'internal-border border-color frame)
;; 						(when (facep 'child-frame-border)
;; 							(set-face-background 'child-frame-border border-color frame))
;; 						(company-box-doc--set-frame-position frame)

;; 						;; Fix hr props. @see `lsp-ui-doc--fix-hr-props'
;; 						(with-current-buffer (company-box--get-buffer "doc")
;; 							(let (next)
;; 								(while (setq next (next-single-property-change (or next 1) 'company-box-doc--replace-hr))
;; 									(when (get-text-property next 'company-box-doc--replace-hr)
;; 										(put-text-property next (1+ next) 'display
;; 																			 '(space :align-to (- right-fringe 1) :height (1)))
;; 										(put-text-property (1+ next) (+ next 2) 'display
;; 																			 '(space :align-to right-fringe :height (1)))))))

;; 						(unless (frame-visible-p frame)
;; 							(make-frame-visible frame))))))
;; 		(advice-add #'company-box-doc--show :override #'my-company-box-doc--show)

;; 		(defun my-company-box-doc--set-frame-position (frame)
;; 			(-let* ((frame-resize-pixelwise t)

;; 							(box-frame (company-box--get-frame))
;; 							(box-position (frame-position box-frame))
;; 							(box-width (frame-pixel-width box-frame))
;; 							(box-height (frame-pixel-height box-frame))
;; 							(box-border-width (frame-border-width box-frame))

;; 							(window (frame-root-window frame))
;; 							((text-width . text-height) (window-text-pixel-size window nil nil
;; 																																	(/ (frame-pixel-width) 2)
;; 																																	(/ (frame-pixel-height) 2)))
;; 							(border-width (or (alist-get 'internal-border-width company-box-doc-frame-parameters) 0))

;; 							(x (- (+ (car box-position) box-width) border-width))
;; 							(space-right (- (frame-pixel-width) x))
;; 							(space-left (car box-position))
;; 							(fringe-left (or (alist-get 'left-fringe company-box-doc-frame-parameters) 0))
;; 							(fringe-right (or (alist-get 'right-fringe company-box-doc-frame-parameters) 0))
;; 							(width (+ text-width border-width fringe-left fringe-right))
;; 							(x (if (> width space-right)
;; 										 (if (> space-left width)
;; 												 (- space-left width)
;; 											 space-left)
;; 									 x))
;; 							(y (cdr box-position))
;; 							(bottom (+ company-box--bottom (frame-border-width)))
;; 							(height (+ text-height (* 2 border-width)))
;; 							(y (cond ((= x space-left)
;; 												(if (> (+ y box-height height) bottom)
;; 														(+ (- y height) border-width)
;; 													(- (+ y box-height) border-width)))
;; 											 ((> (+ y height) bottom)
;; 												(- (+ y box-height) height))
;; 											 (t y))))
;; 				(set-frame-position frame (max x 0) (max y 0))
;; 				(set-frame-size frame text-width text-height t)))
;; 		(advice-add #'company-box-doc--set-frame-position :override #'my-company-box-doc--set-frame-position))

;; 	(when (require 'all-the-icons nil t)
;; 		(declare-function all-the-icons-faicon 'all-the-icons)
;; 		(declare-function all-the-icons-material 'all-the-icons)
;; 		(declare-function all-the-icons-octicon 'all-the-icons)
;; 		(setq company-box-icons-all-the-icons
;; 					`((Unknown . ,(all-the-icons-material "find_in_page" :height 1.0 :v-adjust -0.2))
;; 						(Text . ,(all-the-icons-faicon "text-width" :height 1.0 :v-adjust -0.02))
;; 						(Method . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
;; 						(Function . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
;; 						(Constructor . ,(all-the-icons-faicon "cube" :height 1.0 :v-adjust -0.02 :face 'all-the-icons-purple))
;; 						(Field . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
;; 						(Variable . ,(all-the-icons-octicon "tag" :height 1.1 :v-adjust 0 :face 'all-the-icons-lblue))
;; 						(Class . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
;; 						(Interface . ,(all-the-icons-material "share" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
;; 						(Module . ,(all-the-icons-material "view_module" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
;; 						(Property . ,(all-the-icons-faicon "wrench" :height 1.0 :v-adjust -0.02))
;; 						(Unit . ,(all-the-icons-material "settings_system_daydream" :height 1.0 :v-adjust -0.2))
;; 						(Value . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-lblue))
;; 						(Enum . ,(all-the-icons-material "storage" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
;; 						(Keyword . ,(all-the-icons-material "filter_center_focus" :height 1.0 :v-adjust -0.2))
;; 						(Snippet . ,(all-the-icons-material "format_align_center" :height 1.0 :v-adjust -0.2))
;; 						(Color . ,(all-the-icons-material "palette" :height 1.0 :v-adjust -0.2))
;; 						(File . ,(all-the-icons-faicon "file-o" :height 1.0 :v-adjust -0.02))
;; 						(Reference . ,(all-the-icons-material "collections_bookmark" :height 1.0 :v-adjust -0.2))
;; 						(Folder . ,(all-the-icons-faicon "folder-open" :height 1.0 :v-adjust -0.02))
;; 						(EnumMember . ,(all-the-icons-material "format_align_right" :height 1.0 :v-adjust -0.2))
;; 						(Constant . ,(all-the-icons-faicon "square-o" :height 1.0 :v-adjust -0.1))
;; 						(Struct . ,(all-the-icons-material "settings_input_component" :height 1.0 :v-adjust -0.2 :face 'all-the-icons-orange))
;; 						(Event . ,(all-the-icons-octicon "zap" :height 1.0 :v-adjust 0 :face 'all-the-icons-orange))
;; 						(Operator . ,(all-the-icons-material "control_point" :height 1.0 :v-adjust -0.2))
;; 						(TypeParameter . ,(all-the-icons-faicon "arrows" :height 1.0 :v-adjust -0.02))
;; 						(Template . ,(all-the-icons-material "format_align_left" :height 1.0 :v-adjust -0.2)))
;; 					company-box-icons-alist 'company-box-icons-all-the-icons)))

(provide 'setup-company)
;; setup-company.el ends here
