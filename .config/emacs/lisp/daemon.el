(provide 'daemon)

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
