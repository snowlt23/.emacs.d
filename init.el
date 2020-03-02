;; UTF-8 settings
(prefer-coding-system 'utf-8-unix)

;; font
;(set-frame-font "-*-Inconsolata-normal-normal-normal-*-*-*-*-*-m-0-iso10646-1")

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
(setq-default tab-width 2
              indent-tabs-mode nil)
; for Makefile
(add-hook 'makefile-mode-hook
  (function (lambda ()
    (setq indent-tabs-mode t))))

;; move point
(global-set-key (kbd "C-;") 'goto-line)
(global-set-key (kbd "C-<up>") '(lambda ()
                              (interactive)
                              (forward-line -10)))
(global-set-key (kbd "C-<down>") '(lambda ()
                              (interactive)
                              (forward-line 10)))

;; window management
(global-unset-key (kbd "C-l"))
(global-set-key (kbd "C-l i") 'split-window-horizontally)
(global-set-key (kbd "C-l o") 'split-window-vertically)
(global-set-key (kbd "C-l x") 'delete-window)
(global-set-key (kbd "C-u") 'other-window)

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

;; org-mode
(setq org-directory (expand-file-name "~/Dropbox/org/"))
(defun org-path (f)
  (concat org-directory f))
(setq org-default-notes-file "notes.org")
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates '(
  ("n" "Note" entry
    (file+headline (lambda () (org-path "notes.org")) "Notes")
    "* %?\nEntered on %U\n %i\n %a")
  ("t" "Todo" entry
    (file+headline (lambda () (org-path "todos.org")) "Todos")
    "* TODO [#B] %?")
))
(defun show-org-buffer (file)
  "Show an org-file FILE on the current buffer."
  (interactive)
  (if (get-buffer file)
      (let ((buffer (get-buffer file)))
        (switch-to-buffer buffer)
        (message "%s" file))
    (find-file (org-path file))))
(global-set-key (kbd "C-c n") '(lambda () (interactive)
                                 (show-org-buffer "notes.org")))
(global-set-key (kbd "C-c t") '(lambda () (interactive)
                                 (show-org-buffer "todos.org")))

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
  (delete-file "~/.emacs.d/isinstalled")
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
