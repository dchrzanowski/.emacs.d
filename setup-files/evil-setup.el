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
  (setq evil-want-integration t
        evil-want-keybinding nil)

  :custom
  (evil-kill-on-visual-paste
   nil
   "Stop `evil-visual-paste' from adding the replaced text to the kill ring.")

  :config
  (evil-mode 1)
  (setq-default evil-move-cursor-back nil
                evil-move-beyond-eol t
                evil-cross-lines t
                evil-echo-state nil)

  ;; treat symbols such as the dash and underscore as a part of a word
  ;; (defalias #'forward-evil-word #'forward-evil-symbol)

  ;; force emacs state in
  (add-to-list 'evil-emacs-state-modes 'pomidor-mode)
  (add-to-list 'evil-emacs-state-modes 'paradox-menu-mode)
  (add-to-list 'evil-emacs-state-modes 'magit-repolist-mode)
  (add-to-list 'evil-emacs-state-modes 'org-brain-visualize-mode)
  (add-to-list 'evil-emacs-state-modes 'cfw:details-mode)
  (add-to-list 'evil-emacs-state-modes 'fzf-projectile)
  (add-to-list 'evil-emacs-state-modes 'transmission-mode)
  (add-to-list 'evil-emacs-state-modes 'image-mode)
  (add-to-list 'evil-emacs-state-modes 'chronos-mode)

  (add-to-list 'evil-motion-state-modes 'fundamental-mode)
  (add-to-list 'evil-motion-state-modes 'tabulated-list-mode)
  (add-to-list 'evil-normal-state-modes 'disk-usage-mode)

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
  ;; NOTE: this is a temp fix to deal with evil-mc messing up registers
  ;; issue is described here https://github.com/gabesoft/evil-mc/issues/83
  (setq evil-mc-cursor-variables
        (mapcar
         (lambda (s)
           (remove 'register-alist
                   (remove 'evil-markers-alist
                           (remove evil-was-yanked-without-register s))))
         evil-mc-cursor-variables))
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

(use-package vimish-fold
  :ensure
  :after evil)

(use-package evil-vimish-fold
  :ensure
  :after vimish-fold
  :init
  (setq evil-vimish-fold-target-modes '(prog-mode conf-mode text-mode))
  :config
  (global-evil-vimish-fold-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init 'ag)
  (evil-collection-init 'arc-mode)
  (evil-collection-init 'bookmark)
  (evil-collection-init 'calendar)
  (evil-collection-init 'dired)
  (evil-collection-init 'disk-usage)
  (evil-collection-init 'eshell)
  (evil-collection-init 'ibuffer)
  (evil-collection-init 'info)
  (evil-collection-init 'ivy)
  (evil-collection-init 'magit)
  (evil-collection-init 'magit-todos)
  (evil-collection-init 'mu4e)
  (evil-collection-init 'occur)
  (evil-collection-init 'popup)
  (evil-collection-init 'proced)
  (evil-collection-init 'profiler)
  (evil-collection-init 'quickrun)
  (evil-collection-init 'realgud)
  (evil-collection-init 'xref)

  ;; bug in evil-collection-occur, does not start automatically
  ;; (require 'evil-collection-occur)
  ;; (evil-collection-occur-setup)
  )

(provide 'evil-setup)
;;; evil-setup.el ends here
