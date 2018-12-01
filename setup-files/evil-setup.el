;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; God mode and evil god-state
;; --------------------------------------------------------------------
(use-package god-mode
  :defer 10
  :config
  (setq god-exempt-major-modes nil
        god-exempt-predicates nil))

(use-package evil-god-state
  :defer 10)

;; --------------------------------------------------------------------
;; EVIL MODE
;; --------------------------------------------------------------------
(use-package evil
  :init
  (setq evil-want-integration nil
        evil-want-keybinding nil)

  :config
  (evil-mode 1)
  (setq-default evil-move-cursor-back nil
                evil-cross-lines t
                evil-echo-state nil)

  ;; treat symbols such as the dash and underscore as a part of a word
  ;; (defalias #'forward-evil-word #'forward-evil-symbol)

  ;; rename states
  (evil-put-property 'evil-state-properties 'normal   :tag " NORMAL ")
  (evil-put-property 'evil-state-properties 'insert   :tag " INSERT ")
  (evil-put-property 'evil-state-properties 'visual   :tag " VISUAL ")
  (evil-put-property 'evil-state-properties 'motion   :tag " MOTION ")
  (evil-put-property 'evil-state-properties 'emacs    :tag " EMACS ")
  (evil-put-property 'evil-state-properties 'replace  :tag " REPLACE ")
  (evil-put-property 'evil-state-properties 'operator :tag " OPERTR ")
  (evil-put-property 'evil-state-properties 'god      :tag " GOD-MODE ")

  ;; force emacs state in
  (add-to-list 'evil-emacs-state-modes 'pomidor-mode)
  (add-to-list 'evil-emacs-state-modes 'paradox-menu-mode)
  (add-to-list 'evil-emacs-state-modes 'magit-repolist-mode)
  (add-to-list 'evil-emacs-state-modes 'org-brain-visualize-mode)
  (add-to-list 'evil-emacs-state-modes 'cfw:details-mode)
  (add-to-list 'evil-emacs-state-modes 'fzf-projectile)

  (add-to-list 'evil-normal-state-modes 'fundamental-mode)

  (eval-after-load 'git-timemachine
    '(progn
       (evil-make-overriding-map git-timemachine-mode-map 'normal)
       (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))))  ;; git-timemachine, switch off evil

(use-package evil-anzu
  :after evil)

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-visualstar
  :after evil
  :config
  (setq evil-visualstar/persistent t)
  (global-evil-visualstar-mode))

(use-package evil-args
  :defer t
  :after evil)

(use-package evil-magit
  :defer 2
  :after evil)

(use-package evil-org
  :diminish 'evil-org-mode
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-set-key-theme '(navigation insert textobjects additional shift todo heading))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))


(use-package evil-mc
  :after evil
  :config
  (setq evil-mc-undo-cursors-on-keyboard-quit t)
  (add-hook 'evil-mc-before-cursors-created (lambda () (setq-default evil-move-cursor-back t)))
  (add-hook 'evil-mc-after-cursors-deleted (lambda () (setq-default evil-move-cursor-back nil)))
  (advice-add 'helm-swoop--edit :after #'evil-mc-mode)
  (advice-add 'helm-ag--edit :after #'evil-mc-mode)
  (global-evil-mc-mode 1))

(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

(use-package evil-nerd-commenter
  :defer t)

(use-package evil-snipe
  :after evil
  :diminish evil-snipe-local-mode
  :config
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'whole-buffer
        evil-snipe-smart-case t)
  (evil-snipe-mode)
  (evil-snipe-override-mode))

(use-package evil-visual-mark-mode
  :after evil
  :config
  (evil-visual-mark-mode))

(use-package evil-exchange
  :after evil
  :config
  (evil-exchange-install))

(use-package evil-numbers
  :after evil)

(use-package evil-ediff
  :after evil)

(use-package evil-indent-plus
  :config
  (evil-indent-plus-default-bindings))

(use-package exato
  :after evil)

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list nil)
  (setq evil-collection-mode-list '(ag
                                    arc-mode
                                    bookmark
                                    calendar
                                    dired
                                    eshell
                                    ibuffer
                                    popup
                                    profiler
                                    quickrun
                                    realgud
                                    xref))
  (evil-collection-init)
  ;; bug in evil-collection-occur, does not start automatically
  (require 'evil-collection-occur)
  (evil-collection-occur-setup))

(provide 'evil-setup)
;;; evil-setup ends here
