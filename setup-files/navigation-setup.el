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
        neo-autorefresh nil
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
;; treemacs
;; --------------------------------------------------------------------
(use-package treemacs
  :defer t)
(use-package treemacs-evil
  :after (treemacs evil))

;; --------------------------------------------------------------------
;; binky
;; --------------------------------------------------------------------
(use-package binky
  :defer t
  :config
  (setq binky-preview-delay 0
        binky-preview-in-groups nil
        ;; binky-preview-order '(back manual recent)
        binky-preview-order '(back manual)
        binky-preview-side 'bottom)
  (binky-mode))

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

  (defun dchrzan/eyebrowse-8th-slot-handler ()
    "Handle switch to the 8th slot. Dired."
    (dchrzan/call-func-when-mode-not-in-windows
     'dired-mode
     '(lambda() (dchrzan/call-func-on-y "Switch to Dired?" 'dchrzan/switch-to-dired-two-panel))))

  (defun dchrzan/eyebrowse-7th-slot-handler ()
    "Handle switch to the 7th slot. Mu4e."
    (dchrzan/call-func-when-mode-not-in-windows
     'mu4e-main-mode
     'dchrzan/switch-to-mu4e))

  (defun dchrzan/eyebrowse-9th-slot-handler ()
    "Handle switch to the 9th slot. Mu4e."
    (dchrzan/call-func-when-mode-not-in-windows
     'emacs-lisp-mode
     '(lambda() (dchrzan/call-func-on-y "Switch to init.el?" 'dchrzan/switch-to-init-el))))

  (defvar dchrzan/eyebrowse-post-slot-switch-configs nil
    "Association List that provides functions that will be executed on specified eyebrowse slots.
ALIST key value pairs represent the eyebrowse-slot and the functions to call, respectively.")

  ;; Configure what to do after switching to certain workspace slots
  (setq dchrzan/eyebrowse-post-slot-switch-configs
        '((8 . dchrzan/eyebrowse-8th-slot-handler)
          ;; (7 . dchrzan/eyebrowse-7th-slot-handler)
          (9 . dchrzan/eyebrowse-9th-slot-handler)
          ))

  (defun dchrzan/eyebrowse-post-slot-switch-handler ()
    "Handles post eyebrowse window config switch."
    (let* ((eyebrowse-slot (eyebrowse--get 'current-slot))
           (post-slot-switch-config (assoc eyebrowse-slot dchrzan/eyebrowse-post-slot-switch-configs)))
      (when (consp post-slot-switch-config)
        (funcall (cdr post-slot-switch-config)))))

  (add-hook 'eyebrowse-post-window-switch-hook 'dchrzan/eyebrowse-post-slot-switch-handler)
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
      (?w kill-current-buffer)
      (?X delete-window)
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
(global-tab-line-mode)

;; --------------------------------------------------------------------
;; zoxide
;; --------------------------------------------------------------------
(use-package zoxide
  :after dired
  :config
  (defun dchrzan/add-to-zoxide-excluding ()
    (let ((dirname (dired-current-directory)))
      (unless (string-match-p "\\(node_modules\\|Downloads\\)" dirname)
        (zoxide-add (dired-current-directory)))))

  (add-hook 'dired-after-readin-hook (lambda () (dchrzan/add-to-zoxide-excluding))))

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
  (add-hook 'ibuffer-hook
            (lambda ()
              (ibuffer-vc-set-filter-groups-by-vc-root)
              ;; sort alphabetically
              ;; (unless (eq ibuffer-sorting-mode 'alphabetic)
              ;;   (ibuffer-do-sort-by-alphabetic))
              )))

(provide 'navigation-setup)
;;; navigation-setup.el ends here
