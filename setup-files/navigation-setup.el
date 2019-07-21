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
;; ranger
;; --------------------------------------------------------------------
(use-package ranger
  :config
  (setq ranger-cleanup-on-disable t
        ranger-footer-delay 0.2
        ranger-preview-delay 0.080
        ranger-modify-header nil
        ranger-show-literal nil))

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
  :defer 5
  :config (setq dumb-jump-selector 'helm))

;; --------------------------------------------------------------------
;; smart-jump
;; --------------------------------------------------------------------
(use-package smart-jump
  :defer 1
  :config
  (smart-jump-setup-default-registers)

  (smart-jump-register :modes 'emacs-lisp-mode
                       :jump-fn 'xref-find-definitions
                       :pop-fn 'pop-tag-mark
                       :should-jump t
                       :heuristic 'error
                       :async nil)

  (smart-jump-register :modes 'typescript-mode
                       :jump-fn 'tide-jump-to-definition
                       :pop-fn 'tide-jump-back
                       :should-jump t
                       :heuristic 'error
                       :async nil)

  (smart-jump-register :modes 'js2-mode
                       :jump-fn 'tern-find-definition
                       :pop-fn 'tern-pop-find-definition
                       :should-jump t
                       :heuristic 'error
                       :async nil)

  (smart-jump-register :modes 'python-mode
                       :jump-fn 'elpy-goto-definition
                       :pop-fn 'pop-tag-mark
                       :refs-fn 'elpy-xref--references
                       :should-jump t
                       :heuristic 'point
                       :async nil))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replace with the below snippet in all-the-icons.el, lines 811 - 818.
;; Fixes incorrect font-lock mappings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         (let ((face (if other-face
;;                         `(:family ,family :height ,height :inherit ,other-face)
;;                       `(:family ,family :height ,height))))
;;           (propertize icon
;;                       'face face           ;so that this works without `font-lock-mode' enabled
;;                       'font-lock-face face ;so that `font-lock-mode' leaves this alone
;;                       'display `(raise ,v-adjust)
;;                       'rear-nonsticky t))))

;; --------------------------------------------------------------------
;; ibuffer
;; --------------------------------------------------------------------
(use-package ibuffer-vc
  :defer 1
  :config
  (setq ibuffer-expert t)
  (remove-hook 'ibuffer-hook
               (lambda ()
                 (ibuffer-vc-set-filter-groups-by-vc-root)
                 ;; sort alphabetically
                 ;; (unless (eq ibuffer-sorting-mode 'alphabetic)
                 ;;   (ibuffer-do-sort-by-alphabetic))
                 )))

(provide 'navigation-setup)
;;; navigation-setup ends here
