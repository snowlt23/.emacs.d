;;;
;;; Packages
;;;

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package evil
  :config
  (evil-mode 1))

;; cursor movement
(evil-define-key '(normal visual) 'global (kbd "d") 'evil-backward-char)
(evil-define-key '(normal visual) 'global (kbd "h") 'evil-next-line)
(evil-define-key '(normal visual) 'global (kbd "t") 'evil-previous-line)
(evil-define-key '(normal visual) 'global (kbd "n") 'evil-forward-char)
(evil-define-key '(normal visual) 'global (kbd "H") 'evil-scroll-page-down)
(evil-define-key '(normal visual) 'global (kbd "T") 'evil-scroll-page-up)

;; basic
(evil-define-key '(normal visual) 'global (kbd "SPC f") 'ido-find-file)
(evil-define-key '(normal visual) 'global (kbd "SPC b") 'ido-switch-buffer)
(evil-define-key '(normal visual) 'global (kbd "SPC s") 'save-buffer)
(evil-define-key '(normal visual) 'global (kbd "c") 'evil-insert)
(evil-define-key '(normal visual) 'global (kbd "r") 'evil-append)
(evil-define-key '(normal visual) 'global (kbd "SPC q q") 'save-buffers-kill-terminal)

;; delete
(evil-define-key 'visual 'global (kbd "e") 'evil-delete)
(evil-define-key 'normal 'global (kbd "e e") 'evil-delete-whole-line)

;; window management
(evil-define-key 'normal 'global (kbd "SPC c") 'split-window-horizontally)
(evil-define-key 'normal 'global (kbd "SPC r") 'split-window-vertically)
(evil-define-key 'normal 'global (kbd "SPC e") 'delete-window)
(evil-define-key 'normal 'global (kbd "SPC d") 'windmove-left)
(evil-define-key 'normal 'global (kbd "SPC h") 'windmove-down)
(evil-define-key 'normal 'global (kbd "SPC t") 'windmove-up)
(evil-define-key 'normal 'global (kbd "SPC n") 'windmove-right)

(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (ido-mode t))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-C-p-up-and-down))

(use-package smex
  :config
  (evil-define-key '(normal visual) 'global (kbd "SPC SPC") 'smex))
