(ido-mode t)
(setq ido-enable-flex-matching t)
(defalias 'yes-or-no-p 'y-or-n-p)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)
(recentf-mode)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(show-paren-mode 1)
(setq-default indent-tabs-mode nil)
(setq x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(load-theme 'tango-dark)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))

(package-initialize)
(global-set-key (kbd "C-x g") 'magit-status)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq inhibit-startup-message t)

;; Add extra package repositories
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/")
             '("marmalade" . "http://marmalade-repo.org/packages/"))

;; List packages we want to always have
(package-initialize)
(setq adamrt-packages
      '(ack-and-a-half
        exec-path-from-shell
        flymake-cursor
        flycheck
        js2-mode
        magit
        smex
        web-mode))


;; Install the packages above if not installed already
(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (pkg adamrt-packages)
  (when (and (not (package-installed-p pkg))
             (assoc pkg package-archive-contents))
    (package-install pkg)))


;; A few settings
(setq user-full-name "Clayton Miller" ;; name for various places
      column-number-mode t ;; show column number on modeline (bar at bottom)
      default-directory "~/" ;; use home directory instead of /usr/lib/Cellar/..
      inhibit-startup-message t ;; don't show emacs startup message
      save-interprogram-paste-before-kill t ;; ask me about this
      scroll-step 1 ;; scroll by one
      truncate-lines t ;; don't wrap lines
      truncate-partial-width-windows nil ;; dont wrap lines
      )

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


;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Filetypes
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.php$" . web-mode))

;; web-mode use django engine for html files
(setq web-mode-engines-alist '(("django" . "\\.html\\'")))

;; Flycheck on Python
(add-hook 'python-mode-hook #'(lambda () (flycheck-mode)))

(defun cleanup-buffer-or-region ()
  "Untabify, indent and delete trailing whitespace from buffer or
region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace))))

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((file (ido-completing-read "Recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

;; This is from misc.el
(defun forward-to-word (arg)
  "Move forward until encountering the beginning of a word. With
argument, do this that many times."
  (interactive "p")
  (or (re-search-forward (if (> arg 0) "\\W\\b" "\\b\\W") nil t arg)
      (goto-char (if (> arg 0) (point-max) (point-min)))))

;; pasteboard issue
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

;; Key bindings
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-f") 'forward-to-word)
(global-set-key (kbd "C-x C-m") 'smex)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c n") 'cleanup-buffer-or-region)
(global-set-key (kbd "C-c y") 'bury-buffer)

;; SCSS mode
(setq scss-compile-at-save nil)
