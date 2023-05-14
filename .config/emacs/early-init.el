;; Better startup performance
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)

;; Minimal UI
(setq-default
 package-native-compile t
 default-frame-alist
 '((tool-bar-lines . 0)
   (menu-bar-lines . 0)
   (undecorated . t)
   (vertical-scroll-bars . nil)
   (horizontal-scroll-bars . nil)))

(setq make-backup-files nil
			lock-files nil)

;; UI settings
(global-visual-line-mode 1)
(add-to-list 'default-frame-alist '(alpha 100 100)) ;; opacity settings
(setq inhibit-startup-message t) ;; don't show startup messages
(scroll-bar-mode -1) ;; no scrollbar
(tool-bar-mode -1) ;; no top bar
(tooltip-mode -1) ;; no tooltip
(menu-bar-mode -1) ;; no menu bar
(set-fringe-mode 15) ;; Set the padding of the inside windows
(setq inhibit-startup-screen t
			initial-scratch-message nil
			sentence-end-double-space nil
			ring-bell-function 'ignore
			frame-resize-pixelwise t)

;; Generale sane defaults
(setq inhibit-startup-buffer-menu t) ;; Don't show *Buffer list* when opening multiple files at the same time.
(setq select-enable-clipboard t) ;; System clipboard
(fset 'yes-or-no-p 'y-or-n-p) ;; No more typing the whole yes or no. Just y or n will do.

;; use common convention for indentation by default
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)

(setq vc-follow-symlinks t) ;; Follow symlinks

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
