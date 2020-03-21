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
(setq org-log-done 'time)

(setq org-todo-keywords
  '((sequence "TODO(t)" "SOMEDAY(s)" "WAITING(w)" "|" "DONE(d)" "CANCELED(c)")))

(setq org-capture-templates
      `(("i" "INBOX" entry (file+headline ,(concat org-default-directory "inbox.org") "INBOX")
         "* %?\n  %i")))

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

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;; org-mode key bindings
(global-set-key (kbd "C-c C-c") 'org-capture)
(global-set-key (kbd "C-c f") 'ido-find-orgs)
(global-set-key (kbd "C-c n") 'org-make-namespace)
(global-set-key (kbd "C-c i s") 'org-screenshot)
(global-set-key (kbd "C-c i d") 'org-download-image)
(global-set-key (kbd "C-c r r") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-c r i") 'org-redisplay-inline-images)
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

(use-package deft
  :bind ("C-c d" . deft)
  :commands (deft)
  :init
  (setq deft-directory org-default-directory
        deft-extensions '("org")
        deft-default-extension "org"
        deft-use-filename-as-title t
        deft-use-filter-string-for-filename t
        deft-recursive t))

(defun my-org-download-link-format-function (filename)
  "The default function of `org-download-link-format-function'."
  (if (and (>= (string-to-number org-version) 9.3)
           (eq org-download-method 'attach))
      (format "[[attachment:%s]]\n"
              (org-link-escape
               (file-relative-name filename (org-attach-dir))))
    (let ((f (org-link-escape (file-name-nondirectory filename))))
      (concat "#+ATTR_ORG: :width 300\n"
              "[[image:" f "]]\n"
              "[[file:Android/media/com.nextcloud.client/nextcloud/admin@osnn.myddns.me%2Fnextcloud/Org/images/" f "]]\n"))))

(use-package org-download
  :init
  (setq org-download-image-dir (concat org-default-directory "images")
        org-download-heading-lvl nil
        org-download-backend "curl \"%s\" -o \"%s\""
        org-download-link-format-function #'my-org-download-link-format-function))

;; org notify

(defvar previous-notify-id nil)

(defun notify (title body)
  (if (eq system-type 'windows-nt)
      (progn
        (if previous-notify-id
          (w32-notification-close previous-notify-id))
        (setq previous-notify-id
              (w32-notification-notify
               :title title
               :body  body)))
    (let ((notify-icon "/usr/share/icons/gnome/32x32/status/appointment-soon.png")
          (notify-sound "/usr/share/sounds/gnome/default/alerts/drip.ogg"))
      (notifications-notify :title title :body body :app-icon notify-icon :sound-file notify-sound))))

(defun notify-wrapper (min-to-app new-time msg)
  (interactive)
  (notify (format "org-agenda: 予定まで%s分" min-to-app)
          msg))

(require 'appt)
(appt-activate 1)
(setq appt-display-format 'window)
(setq appt-display-mode-line nil)
(setq appt-message-warning-time 10)
(setq appt-display-interval appt-message-warning-time)
(setq appt-disp-window-function #'notify-wrapper)
(add-hook 'after-save-hook
          '(lambda ()
             (if (string= (file-name-extension (buffer-file-name)) ".org")
                 (org-agenda-to-appt))))
(org-agenda-to-appt)
(add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt)
