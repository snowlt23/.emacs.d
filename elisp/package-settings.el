;;; ido
(require 'ido)
(require 'ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;;; multi cursor with smartrep
(require 'multiple-cursors)
(require 'smartrep)
(declare-function smartrep-define-key "smartrep")
(global-set-key (kbd "C-M-c")
                'mc/edit-lines)
(global-set-key (kbd "C-M-r")
                'mc/mark-all-in-region)
(global-unset-key (kbd "C-v"))
(smartrep-define-key global-map
    "C-v"
  '(("C-v" . 'mc/mark-next-like-this)
    ("h" . 'mc/mark-next-like-this)
    ("t" . 'mc/mark-previous-like-this)
    ("m" . 'mc/mark-more-like-this-extended)
    ("u" . 'mc/unmark-next-like-this)
    ("U" . 'mc/unmark-previous-like-this)
    ("s" . 'mc/skip-to-next-like-this)
    ("S" . 'mc/skip-to-previous-like-this)
    ("a" . 'mc/mark-all-like-this)
    ("d" . 'mc/mark-all-like-this-dwim)
    ("i" . 'mc/insert-numbers)
    ("o" . 'mc/sort-regions)
    ("O" . 'mc/reverse-regions)))

;;; org-mode
(add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))
(setq org-startup-with-inline-images t)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;;; howm-mode
(require 'howm-mode)
(setq howm-home-directory "~/Nextcloud/howm/")
(global-set-key (kbd "C-c , ,") 'howm-menu)

;;; C
(setq c-default-style "bsd"
      c-basic-offset 2)
(electric-pair-mode t)
