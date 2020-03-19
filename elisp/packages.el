;;;
;;; Packages
;;;

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package ido
  :config
  (setq ido-enable-flex-matching t)
  (ido-mode t))

(use-package ido-vertical-mode
  :config
  (ido-vertical-mode 1)
  (setq ido-vertical-define-keys 'C-n-and-C-p-only))

(use-package smex
  :config
  (global-set-key (kbd "M-x") 'smex))

(use-package smartrep)

(use-package multiple-cursors
  :config
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
      ("O" . 'mc/reverse-regions))))

;;;
;;; org-mode
;;;

(setq org-default-directory "~/Nextcloud/Org/")
(setq org-image-actual-width nil)
(setq org-startup-with-inline-images t)
(setq org-agenda-files (list org-default-directory))

(setq org-capture-templates
      `(("t" "Todo" entry (file+headline ,(concat org-default-directory "todo.org") "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("n" "Note" entry (file+headline ,(concat org-default-directory "notes.org") "Notes")
         "* %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree ,(concat org-default-directory "journal.org"))
         "* %?\nEntered on %U\n  %i\n  %a")))

(defun ido-find-orgs ()
  (interactive)
  (cd org-default-directory)
  (ido-find-file))

(defun org-make-namespace (ns)
  (interactive "sEnter namespace: ")
  (make-directory (concat org-default-directory ns)))

(defun org-screenshot ()
  "Take a screenshot into a time stamped unique-named file in the
same directory as the org-buffer and insert a link to this file."
  (interactive)
  (setq filename (concat (format-time-string "%Y%m%d_%H%M%S") ".png"))
  ;; (call-process (expand-file-name "~/private/scripts/elementary-shot") nil t nil (expand-file-name (concat "~/Nextcloud/Org/images/" filename)))
  (call-process-shell-command (concat "gnome-screenshot -a -f " (expand-file-name (concat org-default-directory "images/" filename))) nil t nil)
  (insert (concat "[[image:" filename "]]"))
  (org-display-inline-images))

(defun org-insert-nslink (l)
  (interactive "sLink: ")
  (insert (concat "[[ns:" l ".org][" l "]]")))

;; org-mode key bindings
(global-set-key (kbd "C-c f") 'ido-find-orgs)
(global-set-key (kbd "C-c n") 'org-make-namespace)
(global-set-key (kbd "C-c i") 'org-screenshot)
(global-set-key (kbd "C-c r") 'org-redisplay-inline-images)
(global-set-key (kbd "C-c o") 'org-open-at-point)
(global-set-key (kbd "C-c k") 'org-insert-nslink)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c h") 'org-promote-subtree)
(global-set-key (kbd "C-c l") 'org-demote-subtree)
(global-set-key (kbd "C-c c i") 'org-clock-in)
(global-set-key (kbd "C-c c o") 'org-clock-out)
(global-set-key (kbd "C-c c r") 'org-clock-report)

(setq org-link-abbrev-alist
      `(("ns" . ,org-default-directory)
        ("image"  . ,(concat org-default-directory "images/"))))

(use-package org-bullets
  :config (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  :init
  (setq org-bullets-bullet-list
        '("⦿" "⊙" "◆" "◇" "❖" "❋"))
  :custom-face
  (org-document-title ((t (:inherit document-title :height 1.7))))
  (org-level-1 ((t (:inherit outline-1 :height 1.5))))
  (org-level-2 ((t (:inherit outline-2 :height 1.2))))
  (org-level-3 ((t (:inherit outline-3 :height 1.0))))
  (org-level-4 ((t (:inherit outline-4 :height 1.0))))
  (org-level-5 ((t (:inherit outline-5 :height 1.0)))))
