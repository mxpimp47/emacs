;; Are we on a mac?
(when (equal system-type 'darwin)
  (exec-path-from-shell-initialize)
  (setq mac-option-modifier 'super
        mac-command-modifier 'meta
        ns-function-modifier 'hyper
        ns-pop-up-frames nil ;; single frame
        ns-use-srgb-colorspace t
        trash-directory "~/.Trash/emacs"
        delete-by-moving-to-trash t))
