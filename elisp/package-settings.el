
;;; company
(require 'company)
(global-company-mode +1)
(setq company-idle-delay 0.4)
(setq company-minimum-prefix-length 2)

;;; ido
(require 'ido)
(require 'ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(require 'smex)
(global-set-key (kbd "M-x")
		'smex)

;;; multi cursor with smartrep
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


;;; undotree
(require 'undo-tree)
(define-key global-map (kbd "C-x u") 'undo-tree-visualize)

;;; smartparens
(require 'smartparens-config)
(smartparens-global-mode t)
(setq sp-highlight-pair-overlay nil)
(defun my-create-newline-and-enter-sexp (&rest _ignored)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

;;; yasnippet
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
(setq nim-nimsuggest-path "~/github/Nim/bin/nimsuggest")
(add-hook 'nim-mode-hook 'nimsuggest-mode)
(add-hook 'nim-mode-hook 'company-mode)
(add-hook 'nimscript-mode-hook 'company-mode)
;; (add-to-list 'auto-indent-multiple-indent-modes 'nim-mode)

;;; C
(setq c-default-style "bsd"
      c-basic-offset 4)
(sp-local-pair 'c-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
