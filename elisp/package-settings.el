;;; doom-theme
(require 'doom-themes)
(load-theme 'doom-one-light t)

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
(setq org-image-actual-width nil)
(setq org-startup-with-inline-images t)
(add-hook 'org-mode-hook 'show-all)

(setq org-default-directory "~/Nextcloud/Org/")

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

(global-set-key (kbd "C-c f") 'ido-find-orgs)
(global-set-key (kbd "C-c n") 'org-make-namespace)
(global-set-key (kbd "C-c i") 'org-screenshot)
(global-set-key (kbd "C-c r") 'org-redisplay-inline-images)

(setq org-link-abbrev-alist
      `(("ns" . org-default-directory)
        ("image"  . ,(concat org-default-directory "images/"))))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
(custom-set-faces
 '(org-document-title ((t (:inherit document-title :height 1.7))))
 '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
 '(org-level-2 ((t (:inherit outline-2 :height 1.3))))
 '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
 '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 )

;;; howm-mode
(setq howm-view-title-header "*")
(require 'howm-mode)
(setq howm-menu-file (concat org-default-directory "menu.txt"))
(add-hook 'howm-view-mode-hook 'org-mode)
(setq howm-directory org-default-directory)
(setq howm-template "* %title\n%date\n\n%cursor")
(global-set-key (kbd "C-c , ,") 'howm-menu)

;;; C
(setq c-default-style "bsd"
      c-basic-offset 2)
(electric-pair-mode t)
