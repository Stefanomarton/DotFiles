;; Better startup performance
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)

;; Minimal UI
(setq-default
 default-frame-alist
 '((tool-bar-lines . 0)
	 (menu-bar-lines . 0)
	 (undecorated . t)
	 (vertical-scroll-bars . nil)
	 (horizontal-scroll-bars . nil)))
