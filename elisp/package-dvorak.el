
;; smex
(global-set-key (kbd "M-q") 'smex)

;; ido
(defun ido-my-keys ()
  (define-key ctl-x-map (kbd "C-i") 'keyboard-escape-quit)
  (define-key ido-completion-map (kbd "C-i") 'keyboard-escape-quit)
  (define-key ido-completion-map (kbd "C-b") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match))
(add-hook 'ido-setup-hook 'ido-my-keys)

;; undotree
(define-key global-map (kbd "C-q u") 'undo-tree-visualize)

;; slime
(define-key slime-mode-map (kbd "C-x") nil)
(define-key slime-mode-map (kbd "C-x") 'backward-char)
