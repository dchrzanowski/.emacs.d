;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; org-mode
;; --------------------------------------------------------------------
(use-package org
  :defer 1
  :config

  ;; --------------------------------------------------------------------
  ;; vars
  ;; --------------------------------------------------------------------
  (setq
   ;; new indentation setup
   org-adapt-indentation t
   ;; open links in the same window
   org-link-frame-setup '((file . find-file))
   ;; org log done headings
   org-log-done t
   ;; start folded, its cleaner
   org-startup-folded t
   ;; don't ask to confirm elisp link launch
   org-confirm-elisp-link-function nil
   ;; don't confirm babel eval
   org-confirm-babel-evaluate nil
   ;; org main dir
   org-directory '("~/GoogleDrive/org")
   ;; org refile location
   org-default-notes-file "~/GoogleDrive/org/refile.org"
   ;; org keywords
   org-todo-keywords '((sequence "VERIFY(v)"
                                 "TODO(t)"
                                 "NEXT(n)"
                                 "IN-PROGRESS(i)"
                                 "|"
                                 "DONE(d)"
                                 "CANCELLED(c)"))
   ;; org refile depth search
   org-refile-targets (quote ((nil :maxlevel . 3) (org-agenda-files :maxlevel . 3)))
   ;; org refile use outline path
   org-refile-use-outline-path t
   org-outline-path-complete-in-steps nil
   ;; org capture templates
   org-capture-templates (quote (("t" "Todo" entry (file "~/GoogleDrive/org/refile.org") "* TODO %?")
                                 ("l" "Link" entry (file "~/GoogleDrive/org/refile.org") "* TODO %?\n  %a")
                                 ("L" "Link with description" entry (file "~/GoogleDrive/org/refile.org") "* TODO %?\n  %A")
                                 ("n" "Note" entry (file "~/GoogleDrive/org/refile.org") "* %?")
                                 ;; ("j" "Journal Note" entry (file+datetree "~/GoogleDrive/org/journal.org") "**** %T %^{Note Title}")
                                 ("j" "Journal Note" entry (file+datetree "~/GoogleDrive/org/journal.org") "* %U %?")
                                 ("w" "Work Journal Note" entry (file+datetree "~/GoogleDrive/org/journal-work.org") "* %U %?")
                                 ("c" "Calfw2org" entry (file "~/GoogleDrive/org/refile.org")  "* %?\n %(cfw:org-capture-day)")))
   ;; --------------------------------------------------------------------
   ;; agenda
   ;; --------------------------------------------------------------------

   org-agenda-show-all-dates nil
   ;; org agenda files location
   org-agenda-files '("~/GoogleDrive/org/projects"
                      "~/GoogleDrive/org")

   ;; org agenda initial span
   org-agenda-span 'month)

  ;; ;; org-babel setup
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot . t)
     (mongo . t)
     (gnuplot . t)
     (shell . t)))

  ;; ;;autorefresh images after org-babel dot evaluations
  (add-hook 'org-babel-after-execute-hook 'auto-refresh-inline-images)

  ;; --------------------------------------------------------------------
  ;; org-tempo
  ;; --------------------------------------------------------------------
  (require 'org-tempo)

  ;; --------------------------------------------------------------------
  ;; org-id
  ;; --------------------------------------------------------------------
  (require 'org-id)
  (setq
   ;; location of org ids
   org-id-locations-file "~/GoogleDrive/org/.org-id-locations"
   ;; ids to work across files
   org-id-track-globally t
   ;; use ID from the drawer for links in exports
   org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
   ;; short IDs
   org-id-method 'org)

  ;; --------------------------------------------------------------------
  ;; org-crypt
  ;; --------------------------------------------------------------------
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key nil))

;; --------------------------------------------------------------------
;; addons
;; --------------------------------------------------------------------
;; org bullets
(use-package org-bullets
  :after org
  :config
  ;; org bullets hook
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; org-helm-rifle
(use-package helm-org-rifle
  :after org
  :defer t)

;; toc-org
(use-package toc-org
  :after org
  :defer 2)

;; org-projectile
(use-package org-projectile
  :after org
  :defer 2
  :config
  (progn
    (setq org-projectile-projects-file
          "~/GoogleDrive/org/projects/projects_refile.org")
    (push (org-projectile-project-todo-entry) org-capture-templates)))

;; org download
(use-package org-download
  :after org
  :defer 2
  :config
  (setq-default org-download-image-dir "./images"))

;; org edna
(use-package org-edna
  :after org
  :defer 2
  :config
  (org-edna-load))

;; org kanban
(use-package org-kanban
  :after org
  :config
  (setq-default org-kanban/layout '("..." . 30)))

;; org-super-agenda
(use-package org-super-agenda
  :after org
  :defer 2
  :config
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
        '((:name "Priority"
                 :and (:tag "Priority" :todo t))
          (:name "Exam"
                 :and (:tag "Exam" :todo t))
          (:name "Assignment"
                 :and (:tag "Assignment" :todo t))
          (:name "Study"
                 :and (:tag "Study" :todo t))
          (:name "Work"
                 :and (:tag "Work" :todo t))
          (:name "Projects"
                 :and (:tag "Projects" :todo t))
          (:name "General"
                 :and (:tag "General" :todo t))
          (:name "Emacs"
                 :and (:tag "Emacs")))))

;; org rainbow tags
(use-package org-rainbow-tags
  :ensure t
  :custom
  (org-rainbow-tags-hash-start-index 1)
  (org-rainbow-tags-adjust-color-percent 10)
  ;; Default is '(:weight 'bold)
  ;; (org-rainbow-tags-extra-face-attributes
  ;;  '(:inverse-video t :box nil :weight 'bold))
  :hook
  (org-mode . org-rainbow-tags-mode))

;; org-fancy-priorities
(use-package org-fancy-priorities
  :ensure t
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  (setq org-fancy-priorities-list '("╾ ➊ ╼"
                                    "╾ ➋ ╼"
                                    "╾ ➌ ╼"
                                    "╾ ▬ ╼")
        org-priority-faces        '((?A . "#fd1a1a")
                                    (?B . "#faff12")
                                    (?C . "#99ff34")
                                    (?D . "#000dff"))))

;; --------------------------------------------------------------------
;; org breadcrumbs
;; --------------------------------------------------------------------
;; written by the kind NickD at (https://emacs.stackexchange.com/questions/61101/keep-displaying-current-org-heading-info-in-some-way)
;; modified slightly  to use org-get-outline-path and a custom root icon
;; TODO: rewrite to use run-with-idle-timer
;;
(defvar dchrzan/breadcrumb-head "◉")
(put-text-property 0 (length dchrzan/breadcrumb-head) 'face 'org-level-1 dchrzan/breadcrumb-head)

(defun dchrzan/org-breadcrumbs ()
  "Get the chain of headings from the top level down to the current heading."
  (if (org-before-first-heading-p)
      ""
    (let ((breadcrumbs (org-format-outline-path
                        (org-get-outline-path 1)
                        (1- (frame-width))
                        nil " > ")))
      (format " %s %s" dchrzan/breadcrumb-head breadcrumbs))))

(defun dchrzan/set-header-line-format()
  "Show breadcrumbs to the current point location in org mode."
  (setq header-line-format '(:eval (dchrzan/org-breadcrumbs))))

(add-hook 'org-mode-hook #'dchrzan/set-header-line-format)


;; --------------------------------------------------------------------
;; org menu
;; --------------------------------------------------------------------
(with-eval-after-load 'org
  (require 'org-menu))

;; --------------------------------------------------------------------
;; ox's
;; --------------------------------------------------------------------
;; org-export to github markdown
(use-package ox-gfm
  :defer t
  :after org)

;; org-export to impress.js
(use-package ox-impress-js
  :defer t
  :after org)

;; org-export reveal.js
(use-package ox-reveal
  :defer t
  :after org)

;; org-export to json
(use-package ox-json
  :defer t
  :after org)

;; --------------------------------------------------------------------
;; ob's
;; --------------------------------------------------------------------
(use-package ob-mongo)

;; --------------------------------------------------------------------
;; org-notify
;; --------------------------------------------------------------------
(with-eval-after-load 'org
  (require 'org-notify)
  (org-notify-add 'default
                  '(:time "-1s" :period "1m" :duration 5 :actions -notify)
                  '(:time "30m" :period "10m" :duration 5 :actions -notify)
                  '(:time "4h" :period "1h" :duration 5 :actions -notify)
                  '(:time "1d" :period "2h" :duration 5 :actions -notify)
                  '(:time "7d" :period "5h" :duration 10 :actions -notify))
  (org-notify-start))

;; --------------------------------------------------------------------
;; calendar
;; --------------------------------------------------------------------
(use-package calfw
  :defer 1
  :config
  (use-package calfw-org
    :config
    (setq cfw:org-capture-template '("c" "calfw2org" entry (file nil)  "* %?\n %(cfw:org-capture-day)")))

  (setq calendar-week-start-day 1
        cfw:display-calendar-holidays t
        cfw:fchar-junction ?╬
        cfw:fchar-vertical-line ?┋
        cfw:fchar-horizontal-line ?┅
        cfw:fchar-left-junction ?╠
        cfw:fchar-right-junction ?╣
        cfw:fchar-top-junction ?╦
        cfw:fchar-top-left-corner ?╔
        cfw:fchar-top-right-corner ?╗))

;; --------------------------------------------------------------------
;; pomidor
;; --------------------------------------------------------------------
(use-package pomidor
  :defer t
  :config
  (setq pomidor-sound-tick nil
        pomidor-seconds 900
        pomidor-break-seconds 600
        pomidor-long-break-seconds 3600
        pomidor-sound-tack nil
        pomidor-sound-overwork nil
        alert-default-style 'libnotify))

;; --------------------------------------------------------------------
;; chronos
;; --------------------------------------------------------------------
(use-package chronos
  :config
  (setq chronos-expiry-functions '(chronos-desktop-notifications-notify)))

;; --------------------------------------------------------------------
;; htmlize
;; --------------------------------------------------------------------
(use-package htmlize)

(provide 'org-setup)
;;; org-setup.el ends here
