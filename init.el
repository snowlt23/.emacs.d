
;; UTF-8 settings
(prefer-coding-system 'utf-8-unix)

;; startup message
(setq inhibit-startup-message t)
;; ring bell
(setq ring-bell-function 'ignore)

;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; scrollbar
(scroll-bar-mode 0)

;; tab
(setq-default tab-width 4
              indent-tabs-mode nil)

;; menu and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; line number
(column-number-mode t)
(global-linum-mode t)
(setq linum-format "%4d ")

(show-paren-mode 1)
(delete-selection-mode t)

;; font
;; (set-face-attribute 'default nil :family "MyricaM M" :height 120)

;; add load path
(add-to-list 'load-path "~/.emacs.d/elisp")

;; custom file
(setq custom-file "~/.emacs.d/elisp/custom.el")
(require 'custom)

;; backup and autosave
(add-to-list 'backup-directory-alist
             (cons "." "~/.emacs.d/backups/"))
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "~/.emacs.d/backups/") t)))
(setq auto-save-timeout 30)
(setq auto-save-interval 100)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)

(setq install-package-list '())
(defun install (package)
  (add-to-list 'install-package-list package))
(defun install-packages ()
  (interactive)
  (package-refresh-contents)
  (dolist (package install-package-list)
    (unless (package-installed-p package)
      (package-install package))))

(defun clean-directory (dir)
  (delete-directory dir t)
  (make-directory dir)
  (write-region "" nil (concat (file-name-as-directory dir) "empty.txt")))
(defun clean-packages ()
  (interactive)
  (clean-directory "~/.emacs.d/elpa"))
(defun clean-backups ()
  (interactive)
  (clean-directory "~/.emacs.d/backups"))
(defun clean-autosaves ()
  (interactive)
  (clean-directory "~/.emacs.d/auto-save-list"))
(defun clean-emacs ()
  (interactive)
  (clean-packages)
  (clean-backups)
  (clean-autosaves))

(load "local-settings.el")

(load "packages.el")
(if (not (file-exists-p "~/.emacs.d/isinstalled"))
    (let ()
      (install-packages)
      (write-region "" nil "~/.emacs.d/isinstalled")))
(load "package-settings.el")

