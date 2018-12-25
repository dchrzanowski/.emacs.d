;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; flyspell
;; --------------------------------------------------------------------
;; enable flyspell in text mode by disable in log mode
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

;; enable flyspell-prog-mode in prog-modes
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; --------------------------------------------------------------------
;; flycheck-posframe
;; --------------------------------------------------------------------
(use-package flycheck-posframe
  :ensure t
  :after flycheck
  :config (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

(provide 'spell-check-setup)
;;; spell-check-setup ends here
