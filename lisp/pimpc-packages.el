(require 'package)

;; set repositories
(setq package-archives '(("melpa" . "http://melpa.milkbox.net/packages/")))

(package-initialize)

;; set desired pkgs
(setq pimpc-packages
      '(ack-and-a-half
        exec-path-from-shell
        flymake-cursor
        flycheck
        js2-mode
        magit
        smex
        web-mode))


;; get pkg list if necessary
(when (not package-archive-contents)
  (package-refresh-contents))

;; install packages
(dolist (pkg pimpc-packages)
  (when (and (not (package-installed-p pkg))
             (assoc pkg package-archive-contents))
    (package-install pkg)))
