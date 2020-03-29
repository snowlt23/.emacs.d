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
(evil-define-key '(normal visual) 'global (kbd "C-e") 'evil-scroll-page-down)
(evil-define-key '(normal visual) 'global (kbd "C-g") 'evil-scroll-page-up)

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

(use-package smartrep)

;;(use-package multiple-cursors
;;  :config
;;  (global-set-key (kbd "C-M-c")
;;                  'mc/edit-lines)
;;  (global-set-key (kbd "C-M-r")
;;                  'mc/mark-all-in-region)
;;  (global-unset-key (kbd "C-v"))
;;  (smartrep-define-key global-map
;;      "C-v"
;;    '(("C-v" . 'mc/mark-next-like-this)
;;      ("h" . 'mc/mark-next-like-this)
;;      ("t" . 'mc/mark-previous-like-this)
;;      ("m" . 'mc/mark-more-like-this-extended)
;;      ("u" . 'mc/unmark-next-like-this)
;;      ("U" . 'mc/unmark-previous-like-this)
;;      ("s" . 'mc/skip-to-next-like-this)
;;      ("S" . 'mc/skip-to-previous-like-this)
;;      ("a" . 'mc/mark-all-like-this)
;;      ("d" . 'mc/mark-all-like-this-dwim)
;;      ("i" . 'mc/insert-numbers)
;;      ("o" . 'mc/sort-regions)
;;      ("O" . 'mc/reverse-regions))))

;;;
;;; org-mode
;;;

(setq org-default-directory "~/Nextcloud/Org/")
(setq org-image-actual-width nil)
(setq org-startup-with-inline-images t)
(setq org-agenda-files (list org-default-directory))
(setq org-log-done 'time)

(evil-set-initial-state 'org-agenda-mode 'normal)
(add-hook 'after-init-hook 'org-agenda-list)

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

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;; org-mode key bindings
(evil-define-key 'normal 'global (kbd "SPC o f") 'ido-find-orgs)
(evil-define-key 'normal 'global (kbd "SPC o i s") 'org-screenshot)
(evil-define-key 'normal 'global (kbd "SPC o i d") 'org-download-image)
(evil-define-key 'normal 'global (kbd "SPC o r r") 'revert-buffer-no-confirm)
(evil-define-key 'normal 'global (kbd "SPC o r i") 'org-redisplay-inline-images)
(evil-define-key 'normal 'global (kbd "SPC o o") 'org-open-at-point)
(evil-define-key 'normal 'global (kbd "SPC o a") 'org-agenda)
(evil-define-key 'normal 'global (kbd "SPC o h") 'org-promote-subtree)
(evil-define-key 'normal 'global (kbd "SPC o l") 'org-demote-subtree)
(evil-define-key 'normal 'global (kbd "SPC o c c") 'org-capture)
(evil-define-key 'normal 'global (kbd "SPC o c i") 'org-clock-in)
(evil-define-key 'normal 'global (kbd "SPC o c o") 'org-clock-out)
(evil-define-key 'normal 'global (kbd "SPC o c r") 'org-clock-report)

(setq org-link-abbrev-alist
      `(("image" . ,(concat org-default-directory "images/"))
        ("nc" . "~/Nextcloud/")))

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
