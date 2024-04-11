;; More than half of the packages I use regularly produce compile warnings. It
;; gets to be quite annoying when the `*Warnings*' window pops up while I'm
;; trying to do work, so we will disable native-comp reporting.
(setq native-comp-async-report-warnings-errors nil)

;; (setq native-comp-jit-compilation t)
;; (setq native-compile-prune-cache t)

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

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq inhibit-compacting-font-caches t)

;; Set scratch buffer mode
(customize-set-variable 'initial-major-mode 'fundamental-mode)
