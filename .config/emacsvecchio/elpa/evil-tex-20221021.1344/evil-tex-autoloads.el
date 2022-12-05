;;; evil-tex-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "evil-tex" "evil-tex.el" (0 0 0 0))
;;; Generated autoloads from evil-tex.el

(autoload 'evil-tex-mode "evil-tex" "\
evil toolbox for LaTeX editing. Provides many text objects
fully utilizing evil-surround, some useful movements, and keymaps
for quickly entering environments or cdlatex-like accents. And
useful toggles.

This is a minor mode.  If called interactively, toggle the
`Evil-Tex mode' mode.  If the prefix argument is positive, enable
the mode, and if it is zero or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is `toggle'.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

To check whether the minor mode is enabled in the current buffer,
evaluate `evil-tex-mode'.

The mode's hook is called both when the mode is enabled and when
it is disabled.

See URL `https://github.com/iyefrat/evil-tex' for the full feature
list.

\(fn &optional ARG)" t nil)

(register-definition-prefixes "evil-tex" '("evil-tex-"))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; evil-tex-autoloads.el ends here
