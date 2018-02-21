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
  (setq
   ;; org log done headings
   org-log-done t
   ;; start folded, its cleaner
   org-startup-folded t

   ;; org main dir
   org-directory '("~/Google Drive/org")
   ;; org refile location
   org-default-notes-file "~/Google Drive/org/refile.org"
   ;; org agenda files location
   org-agenda-files '("~/Google Drive/org/projects/myLectures"
                      "~/Google Drive/org/projects"
                      "~/Google Drive/org")
   ;; org keywords
   org-todo-keywords '((sequence "VERIFY(v)"
                                 "TODO(t)"
                                 "NEXT(n)"
                                 "IN-PROGRESS(i)"
                                 "|"
                                 "DONE(d)"
                                 "DELEGATED(l)"
                                 "CANCELLED(c)"))
   ;; org refile depth search
   org-refile-targets (quote ((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
   ;; org refile use outline path
   org-refile-use-outline-path t
   ;; org capture templates
   org-capture-templates (quote (("t" "todo" entry (file "~/Google Drive/org/refile.org") "* TODO %?")
                                 ("n" "note" entry (file "~/Google Drive/org/refile.org") "* %?")
                                 ("c" "calfw2org" entry (file "~/Google Drive/org/refile.org")  "* %?\n %(cfw:org-capture-day)")
                                 ))
   ;; org agenda initial span
   org-agenda-span 'month
   ;; don't confirm babel eval
   org-confirm-babel-evaluate nil)

  ;; org-babel setup
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (dot . t)))

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
    (setq org-brain-path "~/Google Drive/org/")
    :config
    ;; track all files
    (setq org-id-track-globally t)
    ;; id locations path
    (setq org-id-locations-file "~/Google Drive/org/.org-id-locations")
    ;; org brain capture template
    (push '("b" "Brain" plain (function org-brain-goto-end)
            "* %i%?" :empty-lines 1)
          org-capture-templates)
    (setq org-brain-visualize-default-choices 'all)
    (setq org-brain-title-max-length 12))

  (use-package toc-org
    :defer 1)

  ;; org-projectile
  (use-package org-projectile
    :config
    (progn
      (setq org-projectile-projects-file
            "~/Google Drive/org/projects/projects.org")
      (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
      (push (org-projectile-project-todo-entry) org-capture-templates)))

  (use-package org-alert
    :defer 1
    :config
    (setq alert-default-style 'libnotify
          org-alert-interval 3600)
    (org-alert-enable))

  ;; org download
  (use-package org-download
    :config
    (setq-default org-download-image-dir "./images"))

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
    :after org))

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
;; markdown
;; --------------------------------------------------------------------
(use-package markdown-mode)

;; --------------------------------------------------------------------
;; graphviz
;; --------------------------------------------------------------------
(use-package graphviz-dot-mode)

(provide 'org-setup)
;;; org-setup ends here
