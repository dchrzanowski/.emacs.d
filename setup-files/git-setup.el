;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; magit
;; --------------------------------------------------------------------
(use-package magit
  :defer 3
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

;; --------------------------------------------------------------------
;; magit-todos
;; --------------------------------------------------------------------
(use-package magit-todos
  :defer t
  :after magit
  :config
  (setq magit-todos-max-items 40)
  ;; (add-hook 'magit-todos-mode-hook
  ;;           (lambda ()
  ;;             (define-key magit-todos-item-section-map (kbd "jT") 'evil-next-visual-line)
  ;;             (define-key magit-todos-item-section-map (kbd "jl") 'evil-next-visual-line)
  ;;             (define-key magit-todos-item-section-map (kbd "j") 'evil-next-visual-line)))
  (magit-todos-mode))

;; --------------------------------------------------------------------
;; keychain
;; --------------------------------------------------------------------
(use-package keychain-environment
  :config
  (keychain-refresh-environment))

;; --------------------------------------------------------------------
;; git-link Emacs package to get the GitHub/GitLab/... URL
;; --------------------------------------------------------------------
(use-package git-link
  :defer t)

;; --------------------------------------------------------------------
;; git-modes for git config files
;; --------------------------------------------------------------------
(use-package git-modes
  :defer 5)

(provide 'git-setup)
;;; git-setup ends here
