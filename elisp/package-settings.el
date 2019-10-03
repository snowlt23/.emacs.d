;;; ido
(require 'ido)
(require 'ido-vertical-mode)
(ido-mode t)
(ido-vertical-mode 1)
(setq ido-enable-flex-matching t)
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map (kbd "C-h") 'ido-next-match)
            (define-key ido-completion-map (kbd "C-t") 'ido-prev-match)))

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

;;; undotree
(require 'undo-tree)
(define-key global-map (kbd "C-x u") 'undo-tree-visualize)

;;; C
(setq c-default-style "bsd"
      c-basic-offset 2)
(electric-pair-mode t)

;;; nasm
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\)$" . nasm-mode))

;;; factor
(require 'factor-mode)
(setq fuel-listener-factor-binary "~/factor/factor")
(setq fuel-listener-factor-image "~/factor/factor.image")
