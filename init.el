(add-to-list 'load-path "~/.emacs.d/lisp/")
(defalias 'yes-or-no-p 'y-or-n-p)

;; hide menubar, scrollbar and toolbar
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(require 'pimpc-packages)
(require 'pimpc-func)
(require 'pimpc-mac)
(require 'uniquify)
(require 'dired-x)
(require 'saveplace)

(ido-mode t)
(setq ido-enable-flex-matching t)
(recentf-mode)
(show-paren-mode 1)
(load-theme 'tango-dark)


(setq-default save-place t)
(setq user-full-name "Clayton Miller"
      apropos-do-all t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      column-number-mode t
      default-directory "~/"
      indent-tabs-mode nil
      inhibit-startup-message t
      mouse-yank-at-point t
      save-interprogram-paste-before-kill t
      save-place-file (concat user-emacs-directory "places")
      scroll-step 1
      truncate-lines t
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      x-select-enable-clipboard t
      x-select-enable-primary t)

;; mode for filetypes
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; hooks
(add-hook 'python-mode-hook #'(lambda () (flycheck-mode)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; bindings
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-c n") 'cleanup-buffer-or-region)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-f") 'forward-to-word)
(define-key dired-mode-map (kbd "-") 'dired-up-directory)

;; mode settings
(setq scss-compile-at-save nil
      web-mode-engines-alist '(("django" . "\\.html\\'")))

;; modifier keys
(when (memq window-system '(mac ns))
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        x-select-enable-clipboard t)
  (run-with-idle-timer 5 nil 'exec-path-from-shell-initialize))
