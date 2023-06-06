
;; More than half of the packages I use regularly produce compile warnings. It
;; gets to be quite annoying when the `*Warnings*' window pops up while I'm
;; trying to do work, so we will disable native-comp reporting.
(setq native-comp-async-report-warnings-errors nil)
;;(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))


;; The traditional dance of calming down the garbage collector during init,
;; as that improves startup times. Taken from Doom Emacs [1].
;;
;; [1]: https://github.com/hlissner/doom-emacs/blob/develop/docs/faq.org#how-does-doom-start-up-so-quickly
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)

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
		            :family "JetBrains Mono Nerd Font"
		            :height 120
		            :weight 'normal
		            :width 'normal)

;; Italic comments
(set-face-attribute 'font-lock-comment-face nil
		            :family "Cartograph"
		            :slant 'italic)
(set-face-attribute 'font-lock-function-name-face nil :slant 'italic)
(set-face-attribute 'font-lock-variable-name-face nil :slant 'italic)

;; Choose a fallback with size comparible to Terminus so that we don't break
;; vterm.
;; (set-face-attribut 'unicode (font-spec :size 10 :name "DejaVu Sans"))
