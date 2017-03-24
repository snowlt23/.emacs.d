
;; UTF-8 settings
(set-language-environment "Japanese")
(set-default 'buffer-file-coding-system 'utf-8-with-signature)

;; startup message
(setq inhibit-startup-message t)
;; ring bell
(setq ring-bell-function 'ignore)

;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)

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

;; theme
(load-theme 'adwaita t)
;; font
;; (set-face-attribute 'default nil :family "MyricaM M" :height 120)

;; move window bind to "C-u"
(define-key global-map (kbd "C-u") 'other-window)
(global-set-key (kbd "C-a")
		'back-to-indentation)

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
;; (package-refresh-contents)
(package-initialize)

(setq my-install-packages
      '(
        company
        ido
        ido-vertical-mode
        smex
        multiple-cursors
        smartrep
        undo-tree
        smartparens
        flycheck
        yasnippet
        auto-indent-mode

        nim-mode ; Nim
        ))

(dolist (package my-install-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; company
(require 'company)
(global-company-mode +1)
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)

;; ido
(require 'ido)
(require 'ido-vertical-mode)
(ido-mode 1)
(setq ido-enable-flex-matching t)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(require 'smex)
(global-set-key (kbd "M-x")
		'smex)

;; multi cursor with smartrep
(require 'multiple-cursors)
(require 'smartrep)
(declare-function smartrep-define-key "smartrep")
(global-set-key (kbd "C-M-c")
                'mc/edit-lines)
(global-set-key (kbd "C-M-r")
                'mc/mark-all-in-region)
(global-unset-key "\C-t")
(smartrep-define-key global-map
    "C-t"
  '(("C-t" . 'mc/mark-next-like-this)
    ("n" . 'mc/mark-next-like-this)
    ("p" . 'mc/mark-previous-like-this)
    ("m" . 'mc/mark-more-like-this-extended)
    ("u" . 'mc/unmark-next-like-this)
    ("U" . 'mc/unmark-previous-like-this)
    ("s" . 'mc/skip-to-next-like-this)
    ("S" . 'mc/skip-to-previous-like-this)
    ("*" . 'mc/mark-all-like-this)
    ("d" . 'mc/mark-all-like-this-dwim)
    ("i" . 'mc/insert-numbers)
    ("o" . 'mc/sort-regions)
    ("O" . 'mc/reverse-regions)))

;; undotree
(require 'undo-tree)
(define-key global-map (kbd "C-x u") 'undo-tree-visualize)

;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)
(setq sp-highlight-pair-overlay nil)
(defun my-create-newline-and-enter-sexp (&rest _ignored)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

;; yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/mysnippets"
        ))
(yas-global-mode 1)
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;;; Nim
(setq nim-nimsuggest-path "~/github/Nim-0.16.0/bin/nimsuggest")
(add-hook 'nim-mode-hook 'nimsuggest-mode)
(add-hook 'nim-mode-hook 'company-mode)
(add-hook 'nimscript-mode-hook 'company-mode)
;; (add-to-list 'auto-indent-multiple-indent-modes 'nim-mode)

;;; C
(setq c-default-style "bsd"
      c-basic-offset 4)
(sp-local-pair 'c-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
