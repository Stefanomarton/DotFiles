
;; More than half of the packages I use regularly produce compile warnings. It
;; gets to be quite annoying when the `*Warnings*' window pops up while I'm
;; trying to do work, so we will disable native-comp reporting.
(setq native-comp-jit-compilation t)
(setq native-compile-prune-cache t)
(setq native-comp-async-report-warnings-errors nil)
;;(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; use proper .cache folder for eln-cache
;; (setq package-user-dir (string-replace ".config" ".cache" package-user-dir))
;; (setcar native-comp-eln-load-path
;;         (string-replace ".config" ".cache" (car native-comp-eln-load-path)))

;; The traditional dance of calming down the garbage collector during init,
;; as that improves startup times. Taken from Doom Emacs [1].
;;
;; [1]: https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly

;; Wait till init to start emacs packages
;; (setq package-enable-at-startup nil)

(defvar me/gc-cons-threshold 100000000)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold me/gc-cons-threshold
                  gc-cons-percentage 0.1)))

;; max memory available for gc when opening minibuffer
(defun me/defer-garbage-collection-h ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun me/restore-garbage-collection-h ()
  ;; Defer it so that commands launched immediately after will enjoy the
  ;; benefits.
  (run-at-time
   1 nil (lambda () (setq gc-cons-threshold me/gc-cons-threshold))))

(add-hook 'minibuffer-setup-hook #'me/defer-garbage-collection-h)
(add-hook 'minibuffer-exit-hook #'me/restore-garbage-collection-h)

(defvar me/-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist me/-file-name-handler-alist)))

;; Fill whatever space the window manager has given us.
(setq frame-resize-pixelwise t)

;; ;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; ;; font. By inhibiting this, we easily halve startup times with fonts that are
;; ;; larger than the system default.
(setq frame-inhibit-implied-resize t)

(menu-bar-mode -1)
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)
(setq inhibit-compacting-font-caches t)

;; Font settings
;; default to utf-8 for all the things
;; (set-charset-priority 'unicode)
;; (setq locale-coding-system 'utf-8
;;   coding-system-for-read 'utf-8
;;   coding-system-for-write 'utf-8)
;; (set-terminal-coding-system 'utf-8)
;; (set-keyboard-coding-system 'utf-8)
;; (set-selection-coding-system 'utf-8)
;; (prefer-coding-system 'utf-8)
;; (setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Set default font
(set-face-attribute 'default nil
		            :family "JuliaMono"
		            :height 135
		            :weight 'normal
		            :width 'normal)
;; ;; Set default font

;; Choose a fallback with size comparible to Terminus so that we don't break
;; vterm.
;; (set-face-attribut 'unicode (font-spec :size 10 :name "DejaVu Sans"))
