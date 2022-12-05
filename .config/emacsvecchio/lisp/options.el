(provide 'options)

(setq user-full-name "Stefano Marton"
      user-mail-address "sstefanomarton@gmail.com")

(setq inhibit-startup-message t) ;; Don't show startup messages
(scroll-bar-mode -1) ;; No scrollbar
(tool-bar-mode -1) ;; No top bar
(tooltip-mode -1) ;; No tooltip
(menu-bar-mode -1) ;; No menu bar
(set-fringe-mode 10) ;; Set the padding of the inside windows

;; Toggle column relative and absolute number in the modeline 
(column-number-mode) 
(setq display-line-numbers-type 'relative) 
(global-display-line-numbers-mode)

;; Highlight the current line
(global-hl-line-mode)

;; ;; Makes *scratch* buffer empty:
;; (setq initial-scratch-message "")   

;; ; Removes *scratch* from buffer after the mode has been set.
;; (defun remove-scratch-buffer ()
;;   (if (get-buffer "*scratch*")
;;       (kill-buffer "*scratch*")))
;; (add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; ; Removes *messages* from the buffer.
;; (setq-default message-log-max nil)
;; (kill-buffer "*Messages*")

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; System clipboard
(setq select-enable-clipboard t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;; No more typing the whole yes or no. Just y or n will do.
(fset 'yes-or-no-p 'y-or-n-p)

;; use common convention for indentation by default
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)

;; Enable indentation+completion using the TAB key.
;; Completion is often bound to M-TAB.
(setq tab-always-indent 'complete)

(setq display-buffer-alist
        `((,(rx bos (or "*Apropos*" "*Help*" "*helpful" "*info*" "*Summary*") (0+ not-newline))
           (display-buffer-reuse-mode-window display-buffer-below-selected)
           (window-height . 0.33)
           (mode apropos-mode help-mode helpful-mode Info-mode Man-mode))))

;; follow symlinks 
(setq vc-follow-symlinks t)

  (setq inhibit-startup-screen t
        initial-scratch-message nil
        sentence-end-double-space nil
        ring-bell-function 'ignore
        frame-resize-pixelwise t)

;; less noise when compiling elisp
(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))
(setq native-comp-async-report-warnings-errors nil)
(setq load-prefer-newer t)

;; default to utf-8 for all the things
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8
      coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Sane auto-close parentheses
  (electric-pair-mode +1)
  (setq electric-pair-preserve-balance nil)
  ;; mode-specific local-electric pairs
  (defconst lc/default-electric-pairs electric-pair-pairs)
  (defun lc/add-local-electric-pairs (pairs)
    "Example usage: 
    (add-hook 'jupyter-org-interaction-mode '(lambda () (set-local-electric-pairs '())))
    "
    (setq-local electric-pair-pairs (append lc/default-electric-pairs pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  ;; disable auto pairing for <  >
  (add-function :before-until electric-pair-inhibit-predicate
                (lambda (c) (eq c ?<   ;; >
                                )))

;; Revome useless files and keep folders clean
(setq user-emacs-directory "~/.cache/emacs")
(use-package no-littering)

;; no-littering doesn't set this by default so we must place
;; auto save files in the same path as it uses for sessions
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))
(setq create-lockfiles nil)


(setq-default fill-column  90) 
(setq display-fill-column-indicator-character "|")

