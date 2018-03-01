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

  (setq helm-split-window-in-side-p t
        helm-echo-input-in-header-line t
        helm-move-to-line-cycle-in-source nil)
  ;; remove magit from the buffer list
  (add-to-list 'helm-boring-buffer-regexp-list "\\`\\magit"))

;; --------------------------------------------------------------------
;; helm-ag
;; --------------------------------------------------------------------
(use-package helm-ag)

;; --------------------------------------------------------------------
;; helm-projectile
;; --------------------------------------------------------------------
(use-package helm-projectile
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
;; helm-system-packages
;; --------------------------------------------------------------------
(use-package helm-system-packages
  :defer t)

;; --------------------------------------------------------------------
;; helm-dash
;; --------------------------------------------------------------------
(use-package helm-dash
  :defer t
  :config
  (setq helm-dash-min-length 2
        helm-dash-enable-debugging nil)

  (defun javascript-dash-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("JavaScript")))
  (add-hook 'js2-mode-hook 'javascript-dash-doc)

  (defun java-dash-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("Java")))
  (add-hook 'java-mode-hook 'java-dash-doc)

  (defun typescript-dash-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("TypeScript" "AngularTS")))
  (add-hook 'typescript-mode-hook 'typescript-dash-doc)

  (defun python3-dash-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("Python 3")))
  (add-hook 'python-mode-hook 'python3-dash-doc)

  (defun html-dash-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("HTML")))
  (add-hook 'web-mode-hook 'html-dash-doc)

  (defun elisp-dash-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("Emacs Lisp")))
  (add-hook 'emacs-lisp-mode-hook 'elisp-dash-doc)

  (defun css-dash-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("CSS")))
  (add-hook 'css-mode-hook 'css-dash-doc))

;; --------------------------------------------------------------------
;; helm-gitignore
;; --------------------------------------------------------------------
(use-package helm-gitignore
  :defer t)

;; --------------------------------------------------------------------
;; helm-google
;; --------------------------------------------------------------------
(use-package helm-google
  :defer t)

;; --------------------------------------------------------------------
;; helm-flx (fuzzy match)
;; --------------------------------------------------------------------
(use-package helm-flx
  :init
  (use-package flx)
  :ensure helm
  :config
  (helm-flx-mode +1)
  (setq-default helm-flx-for-helm-find-files t
                helm-flx-for-helm-locate t
                helm-buffers-fuzzy-matching nil
                helm-recentf-fuzzy-match nil
                helm-semantic-fuzzy-match t
                helm-imenu-fuzzy-match t))

;; --------------------------------------------------------------------
;; helm-fzf
;; --------------------------------------------------------------------
(use-package fzf
  :config
  (require 'helm-fzf))

(provide 'helm-setup)
;;; helm-setup ends here
