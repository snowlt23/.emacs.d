
;; dvorak
(setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))

(global-set-key (kbd "C-q") nil)
(global-set-key (kbd "C-x") nil)

(global-set-key (kbd "C-i") 'keyboard-quit)
(global-set-key (kbd "C-z") 'undo)

(global-set-key (kbd "C-p") 'previous-line)
(global-set-key (kbd "C-b") 'next-line)
(global-set-key (kbd "C-u") 'forward-char)
(global-set-key (kbd "C-x") 'backward-char)
(global-set-key (kbd "C-a") 'back-to-indentation)
(global-set-key (kbd "C-.") 'move-end-of-line)
(global-set-key (kbd "C-e") 'delete-char)
(global-set-key (kbd "<tab>") 'indent-according-to-mode)

(global-set-key (kbd "C-,") 'kill-region)
(global-set-key (kbd "M-,") 'kill-ring-save)
(global-set-key (kbd "C-f") 'cua-paste)
(global-set-key (kbd "C-q d") 'mark-whole-buffer)
(global-set-key (kbd "C-q x") 'ido-switch-buffer)
(global-set-key (kbd "C-q C-u") 'ido-find-file)

(global-set-key (kbd "C-q C-o") 'save-buffer)
(global-set-key (kbd "C-q C-j") 'save-buffers-kill-terminal)

(global-set-key (kbd "C-q 0") 'delete-window)
(global-set-key (kbd "C-q 1") 'delete-other-windows)
(global-set-key (kbd "C-q 2") 'split-window-below)
(global-set-key (kbd "C-q 3") 'split-window-right)

;; isearch
(global-set-key (kbd "C-o") 'isearch-forward)
(define-key isearch-mode-map (kbd "C-i") 'isearch-abort)

;; move window bind to "C-u"
(global-set-key (kbd "C-g") 'other-window)
