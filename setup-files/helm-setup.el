;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; helm
;; --------------------------------------------------------------------
(use-package helm
  :config
  (helm-mode t)

  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)

  (unless (boundp 'completion-in-region-function)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

  (setq helm-split-window-inside-p t
        helm-echo-input-in-header-line t
        helm-move-to-line-cycle-in-source nil)
  ;; remove magit from the buffer list
  (add-to-list 'helm-boring-buffer-regexp-list "\\`\\magit"))

;; --------------------------------------------------------------------
;; helm-posframe
;; --------------------------------------------------------------------
;; (use-package helm-posframe
;;   :config
;;   (helm-posframe-enable)
;;   (setq helm-posframe-poshandler 'posframe-poshandler-point-bottom-left-corner
;;         helm-posframe-width '150
;;         helm-posframe-parameters '((internal-border-width . 1)
;;                                    (left-fringe . 5)
;;                                    (right-fringe . 5))))

;; --------------------------------------------------------------------
;; helm-ag
;; --------------------------------------------------------------------
(use-package helm-ag)

;; --------------------------------------------------------------------
;; helm-projectile
;; --------------------------------------------------------------------
(use-package helm-projectile
  :after projectile
  :config
  (projectile-mode)
  ;; (setq projectile-indexing-method 'native)
  (setq-default projectile-enable-caching t
                projectile-completion-system 'helm)
  (helm-projectile-on))

;; --------------------------------------------------------------------
;; helm-swoop
;; --------------------------------------------------------------------
(use-package helm-swoop
  :config
  (setq helm-swoop-split-with-multiple-windows t
        helm-swoop-use-fuzzy-match nil
        helm-swoop-pre-input-function (lambda () "")))

;; --------------------------------------------------------------------
;; helm-gitignore
;; --------------------------------------------------------------------
;; TEMP: disabled waiting for PR https://github.com/jupl/helm-gitignore/pull/6 to be merged
;; due to gitattributes-mode gitconfig-mode gitignore-mode getting merged into a single package
;; (use-package helm-gitignore
;;   :defer t)

;; --------------------------------------------------------------------
;; helm-firefox (provides firefox bookmarks search)
;; --------------------------------------------------------------------
(use-package helm-firefox
  :defer t)

;; --------------------------------------------------------------------
;; helm-fzf
;; --------------------------------------------------------------------
(use-package fzf
  :config
  (require 'helm-fzf))

(provide 'helm-setup)
;;; helm-setup.el ends here
