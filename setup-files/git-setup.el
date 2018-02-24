;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; magit
;; --------------------------------------------------------------------
(use-package magit
  :defer t
  :config
  (setq magit-repository-directories '( ("~/github" . 1) )))

;; --------------------------------------------------------------------
;; git gutter
;; --------------------------------------------------------------------
(use-package git-gutter-fringe
  :defer 5
  :init
  (use-package fringe-helper)
  (fringe-helper-define 'git-gutter-fr:added nil
    "........"
    "....X..."
    "....X..."
    "..XXXXX."
    "....X..."
    "....X..."
    "........"
    "........")
  (fringe-helper-define 'git-gutter-fr:deleted nil
    "........"
    "........"
    "........"
    ".XXXXXX."
    ".XXXXXX."
    "........"
    "........"
    "........")
  (fringe-helper-define 'git-gutter-fr:modified nil
    "........"
    ".XXXXXX."
    ".X....X."
    ".X....X."
    ".X....X."
    ".X....X."
    ".XXXXXX."
    "........")
  :config
  (setq-default git-gutter-fr:side 'right-fringe
                git-gutter:update-interval 1)
  (set-face-foreground 'git-gutter-fr:modified "DarkOrange")
  (set-face-foreground 'git-gutter-fr:added    "OliveDrab")
  (set-face-foreground 'git-gutter-fr:deleted  "firebrick"))

;; --------------------------------------------------------------------
;; git messenger
;; --------------------------------------------------------------------
(use-package git-messenger
  :defer 5
  :init
  (custom-set-variables
   '(git-messenger:use-magit-popup t)
   '(git-messenger:show-detail t)))

;; --------------------------------------------------------------------
;; git-timemachine
;; --------------------------------------------------------------------
(use-package git-timemachine
  :defer 5)

(provide 'git-setup)
;;; git-setup ends here
