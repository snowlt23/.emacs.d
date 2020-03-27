;; UTF-8 settings
(set-locale-environment nil)
(set-language-environment "Japanese")
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(prefer-coding-system 'utf-8)

;; GC
(setq gc-cons-threshold (* 511 1024 1024 ))
(setq gc-cons-percentage 0.5)
(run-with-idle-timer 5 t #'garbage-collect)
(setq garbage-collection-messages t)

;; Startup message
(setq inhibit-startup-message t)
;; ring bell
(setq ring-bell-function 'ignore)

;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; scrollbar
(scroll-bar-mode 0)

;; tab
(setq-default tab-width 2
              indent-tabs-mode nil)
;; for Makefile
(add-hook 'makefile-mode-hook
  (function (lambda ()
    (setq indent-tabs-mode t))))

;; move point
(defun smart-line-beginning ()
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

;; menu and tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; line number
(column-number-mode t)
(global-linum-mode t)
(setq linum-format "%4d ")

(show-paren-mode 1)
(delete-selection-mode t)

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

;;; C
(setq c-default-style "bsd"
      c-basic-offset 2)
(electric-pair-mode t)

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
  (delete-file "~/.emacs.d/isinstalled")
  (clean-packages)
  (clean-backups)
  (clean-autosaves))

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(if (not (file-exists-p "~/.emacs.d/isinstalled"))
  (progn
    (package-refresh-contents)
    (package-install 'use-package)
    (write-region "" nil "~/.emacs.d/isinstalled")))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(load "packages.el")
