;;; pywal-theme.el --- My personal pywal theme

(require 'ewal)

(deftheme pywal "pywal theme")

(let (
      (bg (ewal-load-color 'background 0))
      (bg1 (ewal-load-color 'background -5))
      (bg2 (ewal-load-color 'background 2 3))
      (fg (ewal-load-color 'foreground 0))
      (comment (ewal-load-color 'comment 0))
      (hl (ewal-load-color 'blue -15 5))
      (hl-alt (ewal-load-color 'green -9 5))
      (red        (ewal-load-color 'red      -1))
      (orange     (ewal-load-color 'red       0))
      (coolred     (ewal-load-color 'cyan     0 -10))
      (green      (ewal-load-color 'green    -1))
      (teal       (ewal-load-color 'green     0))
      (yellow     (ewal-load-color 'yellow   -1))
      (blue       (ewal-load-color 'blue      0))
      (dark-blue  (ewal-load-color 'blue     -1))
      (magenta    (ewal-load-color 'magenta   0))
      (violet     (ewal-load-color 'magenta  -1))
      (cyan       (ewal-load-color 'cyan      0))
      (dark-cyan  (ewal-load-color 'cyan     -1))
      (red-alt        (ewal-load-color 'red      5 5))
      (orange-alt     (ewal-load-color 'red      5 5))
      (coolred-alt     (ewal-load-color 'cyan    5 5))
      (green-alt      (ewal-load-color 'green    5 5))
      (teal-alt       (ewal-load-color 'green    5 5))
      (yellow-alt     (ewal-load-color 'yellow   5 5))
      (blue-alt       (ewal-load-color 'blue     5 5))
      (dark-alt-blue  (ewal-load-color 'blue     5 5))
      (magenta-alt    (ewal-load-color 'magenta  5 5))
      (violet-alt     (ewal-load-color 'magenta  5 5))
      (cyan-alt       (ewal-load-color 'cyan     5 5))
      (dark-alt-cyan  (ewal-load-color 'cyan     5 5))
      )

  (custom-theme-set-faces
   'pywal
   ;; Set basic faces
   `(default ((t (:family "JuliaMono" :background ,bg :foreground ,fg))))
   `(fringe ((t (:foreground ,fg :background ,bg))))

   `(hl-line ((t (:background ,hl))))
   `(region ((t (:foreground ,violet :background ,hl-alt))))

   `(mode-line ((t (:height 140 :foreground ,fg :background ,hl :box (:line-width 20 :color ,bg)))))
   `(mode-line-inactive ((t (:height 140 :foreground ,comment :background ,bg :box (:line-width 20 :color ,bg)))))
   ;; '(mode-line-buffer-id ((t (:box (:line-width 1 :color ,fg) :weight bold))))

   `(header-line ((t (:height 160 :foreground ,fg :background ,hl :box (:line-width 20 :color ,bg)))))

   `(font-lock-comment-face ((t (:family "JuliaMono Light" :foreground ,comment :slant oblique))))
   `(font-lock-builtin-face ((t (:foreground ,violet))))
   `(font-lock-comment-delimiter-face ((t (:foreground ,red))))
   `(font-lock-constant-face ((t (:foreground ,green))))
   `(font-lock-doc-face ((t (:foreground ,yellow))))
   `(font-lock-function-name-face ((t (:foreground ,teal))))
   `(font-lock-keyword-face ((t (:weight bold :foreground ,magenta))))
   `(font-lock-negation-char-face ((t (:foreground ,orange))))
   `(font-lock-preprocessor-face ((t (:foreground ,blue :weight bold))))
   `(font-lock-reference-face ((t (:foreground ,green))))
   `(font-lock-regexp-grouping-backslash ((t (:foreground ,red))))
   `(font-lock-regexp-grouping-construct ((t (:foreground ,dark-blue))))
   `(font-lock-string-face ((t (:foreground ,magenta))))
   `(font-lock-type-face ((t (:foreground ,yellow))))
   `(font-lock-variable-name-face ((t (:foreground ,orange))))
   `(font-lock-warning-face ((t (:foreground ,green))))

   ;; `(comint-highlight-prompt ((t (:foreground ,green))))
   `(button ((t (:foreground ,green))))
   `(escape-glyph ((t (:foreground ,green))))
   `(link ((t (:foreground ,green :underline t))))

   `(minibuffer-prompt ((t (:foreground ,green))))

   `(vertico-current ((t (:foreground ,fg :background ,hl :weight bold))))
   `(vertico-group-separator ((t (:foreground ,red :weight bold))))
   ;; `(vertico-group-title ((t (:foreground :background))))
   ;; `(vertico-multiline ((t (:foreground :background))))

   `(completions-annotations ((t (:foreground ,comment ))))
   `(completions-common-part ((t (:foreground ,yellow))))
   `(completions-first-difference ((t (:foreground ,yellow))))
   `(completions-group-separator ((t (:foreground ,green))))
   `(completions-group-title ((t (:foreground ,red))))
   `(completions-highlight ((t (:foreground ,blue))))
   ;; `(completions-first-difference ((t (:foreground ,blue))))

   ;; `(consult-preview-insertion ((t (:foreground ,green :background ,green))))
   ;; `(consult-preview-line ((t (:foreground ,blue :background ,blue))))
   ;; `(consult-preview-match ((t (:foreground ,blue :background ,blue))))

   ;; `(consult-highlight-mark((t (:foreground ,blue :background ,blue))))
   ;; `(consult-highlight-match((t (:foreground ,blue :background ,blue))))
   `(orderless-match-face-0 ((t (:foreground ,blue ))))
   `(orderless-match-face-1 ((t (:foreground ,red ))))
   `(orderless-match-face-2 ((t (:foreground ,green ))))
   `(orderless-match-face-3 ((t (:foreground ,yellow))))
   `(orderless-match-face-4 ((t (:foreground ,blue))))

   '(vertico-posframe-border ((t (:inherit hl-line))))
   '(vertico-posframe ((t (:inherit mode-line))))

   `(window-divider ((t (:foreground ,bg))))
   `(window-divider-first-pixel ((t (:foreground ,bg))))
   `(window-divider-last-pixel ((t (:foreground ,bg))))
   `(vertical-border ((t (:foreground ,bg))))

   `(show-paren-match-face ((t (:foreground ,red :background ,cyan))))
   `(show-paren-mismatch-face ((t (:background ,red))))
   `(show-paren-match ((t (:foreground ,red :background ,cyan))))
   `(show-paren-mismatch ((t (:background ,red))))

   `(rainbow-delimiters-depth-1-face ((t (:foreground ,red-alt))))
   `(rainbow-delimiters-depth-2-face ((t (:foreground ,orange-alt))))
   `(rainbow-delimiters-depth-3-face ((t (:foreground ,coolred-alt))))
   `(rainbow-delimiters-depth-4-face ((t (:foreground ,green-alt))))
   `(rainbow-delimiters-depth-5-face ((t (:foreground ,teal-alt ))))
   `(rainbow-delimiters-depth-6-face ((t (:foreground ,yellow-alt))))
   `(rainbow-delimiters-depth-7-face ((t (:foreground ,blue-alt ))))
   `(rainbow-delimiters-depth-8-face ((t (:foreground ,dark-alt-blue))))
   `(rainbow-delimiters-depth-9-face ((t (:foreground ,magenta-alt))))
   `(rainbow-delimiters-mismatched-face ((t (:foreground ,violet-alt ))))
   `(rainbow-delimiters-unmatched-face ((t (:foreground ,cyan-alt ))))

   `(org-document-title ((t (:foreground ,yellow :weight ultra-bold))))
   `(org-level-1 ((t (:foreground ,blue :weight ultra-bold :height 160))))
   `(org-level-2 ((t (:foreground ,green :weight ultra-bold :height 160))))
   `(org-level-3 ((t (:foreground ,green :weight ultra-bold :height 150))))
   `(org-level-4 ((t (:foreground ,green :weight ultra-bold :height 140))))
   `(org-level-5 ((t (:foreground ,green :weight ultra-bold :height 130))))
   `(org-level-6 ((t (:foreground ,green :weight ultra-bold :height 120))))
   `(org-level-7 ((t (:foreground ,green :weight ultra-bold :height 110))))
   `(org-level-8 ((t (:foreground ,green :weight ultra-bold))))
   `(org-ellipsis ((t (:foreground ,green :weight ultra-bold))))
   `(org-outline-path-headerline-face ((t (:foreground ,red :weight ultra-bold ))))
   `(org-drawer ((t (:foreground ,blue))))
   `(org-date ((t (:foreground ,blue))))
   '(yas-field-highlight-face ((t (:inherit t))))


   (custom-theme-set-variables
    'pywal
    ;; Set variables as needed
    )

   ))

(provide-theme 'pywal)
;;; nord-theme.el ends here
