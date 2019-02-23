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

  ;; old org style template expand
  (require 'org-tempo)

  ;; --------------------------------------------------------------------
  ;; ob's
  ;; --------------------------------------------------------------------
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  ;; GPG key to use for encryption
  ;; Either the Key ID or set to nil to use symmetric encryption.
  (setq org-crypt-key nil)

  ;; --------------------------------------------------------------------
  ;; ob's
  ;; --------------------------------------------------------------------
  (use-package ob-mongo)

  ;; --------------------------------------------------------------------
  ;; vars
  ;; --------------------------------------------------------------------
  (setq
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
                                 ("c" "Calfw2org" entry (file "~/GoogleDrive/org/refile.org")  "* %?\n %(cfw:org-capture-day)")))
   ;; --------------------------------------------------------------------
   ;; agenda
   ;; --------------------------------------------------------------------
   ;; org agenda files location
   org-agenda-show-all-dates nil
   org-agenda-files '("~/GoogleDrive/org/projects"
                      "~/GoogleDrive/org/projects/prefarm"
                      "~/GoogleDrive/org/projects/mtml"
                      "~/GoogleDrive/org")

   ;; org agenda initial span
   org-agenda-span 'month)

  ;; org-babel setup
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot . t)
     (mongo . t)
     (ledger . t)
     (shell . t)))

  ;;autorefresh images after org-babel dot evaluations
  (add-hook 'org-babel-after-execute-hook 'auto-refresh-inline-images)

  ;; --------------------------------------------------------------------
  ;; addons
  ;; --------------------------------------------------------------------
  ;; org bullets
  (use-package org-bullets
    :config
    ;; org bullets hook
    (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

  ;; org-helm-rifle
  (use-package helm-org-rifle
    :defer t)

  ;; org-brain
  (use-package org-brain
    :defer t
    :init
    ;; org brain main search path
    (setq org-brain-path "~/GoogleDrive/org/")
    :config
    ;; track all files
    (setq org-id-track-globally t)
    ;; id locations path
    (setq org-id-locations-file "~/GoogleDrive/org/.org-id-locations")
    ;; org brain capture template
    (push '("b" "Brain" plain (function org-brain-goto-end)
            "* %i%?" :empty-lines 1)
          org-capture-templates)
    (setq org-brain-visualize-default-choices 'all)
    (setq org-brain-title-max-length 12))

  (use-package toc-org
    :defer 2)

  ;; org-projectile
  (use-package org-projectile
    :defer 2
    :config
    (progn
      (setq org-projectile-projects-file
            "~/GoogleDrive/org/projects/projects_refile.org")
      ;; (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
      (push (org-projectile-project-todo-entry) org-capture-templates)))

  ;; org download
  (use-package org-download
    :defer 2
    :config
    (setq-default org-download-image-dir "./images"))

  ;; org edna
  (use-package org-edna
    :defer 2
    :config
    (org-edna-load))

  ;; org kanban
  (use-package org-kanban
    :config
    (setq-default org-kanban/abbreviation '("..." . 20)))

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

  ;; org-super-agenda
  (use-package org-super-agenda
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
                   :and (:tag "Emacs"))))))

;; --------------------------------------------------------------------
;; org-notify
;; --------------------------------------------------------------------
(with-eval-after-load 'org
  (require 'org-notify)
  (org-notify-add 'default
                  '(:time "-1s" :period "1m" :duration 5 :actions -notify )
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
        cfw:fchar-junction ?╋
        cfw:fchar-vertical-line ?┃
        cfw:fchar-horizontal-line ?━
        cfw:fchar-left-junction ?┣
        cfw:fchar-right-junction ?┫
        cfw:fchar-top-junction ?┯
        cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-right-corner ?┓))

;; --------------------------------------------------------------------
;; pomidor
;; --------------------------------------------------------------------
(use-package pomidor
  :defer t
  :config
  (setq pomidor-sound-tick nil
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
;; markdown
;; --------------------------------------------------------------------
(use-package markdown-mode)

;; --------------------------------------------------------------------
;; graphviz
;; --------------------------------------------------------------------
(use-package graphviz-dot-mode)

;; --------------------------------------------------------------------
;; htmlize
;; --------------------------------------------------------------------
(use-package htmlize)


(provide 'org-setup)
;;; org-setup ends here
