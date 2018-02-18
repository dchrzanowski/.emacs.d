;;; package --- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
(put 'dired-find-alternate-file 'disabled nil)  ;; use single window

(setq dired-dwim-target t
      dired-auto-revert-buffer t
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-omit-verbose nil)

;; sort files and show sizes
;; (setq dired-listing-switches "-alhvF --group-directories-first")
(setq dired-listing-switches "-aBhl --group-directories-first")

;; dired async
(dired-async-mode)

;; find files quicker
(use-package dired-narrow)

;; show sizes of subdirs and dirs / file
(use-package dired-du
  :config
  (setq dired-du-size-format t))

;; extras TODO
(use-package dired-hacks-utils)

;; reuse single dired buffer
(use-package dired-single)

;; show/hide dotfiles
(use-package dired-hide-dotfiles
  :config
  (add-hook 'dired-mode-hook #'dired-hide-dotfiles-mode))

;; launch dired from point
(use-package dired-launch
  :config
  (dired-launch-enable)
  (setq-default dired-launch-default-launcher '("xdg-open")))

(use-package diredful
  :config
  (diredful-mode 1))

;; --------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------
;; truncate lines
(add-hook 'dired-after-readin-hook (lambda () (progn
                                                (dired-hide-details-mode)
                                                (setq truncate-partial-width-windows t
                                                      truncate-lines t))))
(add-hook 'dired-mode-hook 'auto-revert-mode)

(provide 'dired-settings-setup)
;;; dired-settings-setup.el ends here
