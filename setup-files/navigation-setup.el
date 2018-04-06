;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; neotree
;; --------------------------------------------------------------------
(use-package neotree
  :defer t
  :config
  ;; override the neo's default function which is buggy when opening helm
  (defun neo-global--do-autorefresh ()
    "Do auto refresh."
    (interactive)
    (let ((cw (selected-window)))
      (when (and (not (eq cw neo-global--window)) (neo-global--window-exists-p))
        (progn
          (neotree-find)
          (select-window cw)))))

  (setq neo-theme 'icons
        neo-smart-open t
        neo-autorefresh t
        neo-force-change-root t
        neo-window-width 35)

  ;; truncate neo lines
  (add-hook 'neo-after-create-hook
            #'(lambda (_)
                (with-current-buffer (get-buffer neo-buffer-name)
                  (make-local-variable 'auto-hscroll-mode)
                  (setq truncate-lines t
                        word-wrap nil
                        auto-hscroll-mode nil)))))

;; --------------------------------------------------------------------
;; avy
;; --------------------------------------------------------------------
(use-package avy
  :config
  (setq-default avy-background t
                avy-timeout-seconds 2))

;; --------------------------------------------------------------------
;; eyebrowse
;; --------------------------------------------------------------------
(use-package eyebrowse
  :config
  (setq-default eyebrowse-wrap-around t)
  (eyebrowse-mode t))

;; --------------------------------------------------------------------
;; ace window
;; --------------------------------------------------------------------
(use-package ace-window
  :defer t
  :init
  (setq-default aw-dispatch-always t
                aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  (defvar aw-dispatch-alist
    '((?x aw-delete-window " Ace - Delete Window")
      (?u aw-delete-window " Ace - Delete Window")
      (?i aw-flip-window " Ace - Swap Window")
      (?b aw-split-window-vert " Ace - Split Vert Window")
      (?v aw-split-window-horz " Ace - Split Horz Window")
      (?r delete-other-windows " Ace - Maximize Window")
      (?o delete-other-windows)
      (?w kill-this-buffer)
      "List of actions for `aw-dispatch-default'.")))

;; --------------------------------------------------------------------
;; ace link
;; --------------------------------------------------------------------
(use-package ace-link
  :defer t
  :config
  (ace-link-setup-default))

;; --------------------------------------------------------------------
;; tabbar
;; --------------------------------------------------------------------
(use-package tabbar
  :config
  (tabbar-mode))


;; --------------------------------------------------------------------
;; dumb-jump
;; --------------------------------------------------------------------
(use-package dumb-jump
  :defer t
  :config (setq dumb-jump-selector 'helm))

;; --------------------------------------------------------------------
;; imenu-anywhere
;; --------------------------------------------------------------------
(use-package imenu-anywhere
  :defer t)

;; --------------------------------------------------------------------
;; all the icons
;; --------------------------------------------------------------------
(use-package all-the-icons)
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; --------------------------------------------------------------------
;; ibuffer
;; --------------------------------------------------------------------
(use-package ibuffer-vc
  :defer 1
  :config
  (setq ibuffer-expert t)
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              (unless (eq ibuffer-sorting-mode 'alphabetic)
                (ibuffer-do-sort-by-alphabetic)))))

(provide 'navigation-setup)
;;; navigation-setup ends here
