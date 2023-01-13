;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; diminish items from the modeline
;; --------------------------------------------------------------------
(diminish 'visual-line-mode)
(diminish 'which-key-mode)
(diminish 'company-mode)
(diminish 'undo-tree-mode)
(diminish 'undo-tree-visualizer-selection-mode)
(diminish 'drag-stuff-mode)
(diminish 'helm-mode)
(diminish 'yas-minor-mode)
(diminish 'projectile-mode)
(diminish 'anzu-mode)
(diminish 'flycheck-mode)
(diminish 'smartparens-mode)
(diminish 'auto-revert-mode)
(diminish 'dired-omit-mode)
(diminish 'all-the-icons-dired-mode)
(diminish 'dired-launch-mode)
(diminish 'tern-mode)
(diminish 'rainbow-mode)
(diminish 'evil-mc-mode)
(diminish 'evil-org-mode)
(diminish 'dired-async-mode)
(diminish 'flyspell-prog-mode)
(diminish 'flyspell-mode)
(diminish 'hi-lock-mode)
(diminish 'highlight-thing-mode)
(diminish 'docker-mode)
(diminish 'highlight-indentation-current-column-mode)
(add-hook 'evil-god-state-entry-hook (lambda () (diminish 'god-local-mode)))
(add-hook 'evil-god-state-exit-hook (lambda () (diminish-undo 'god-local-mode)))

(provide 'diminish-setup)
;;; diminish-setup.el ends here
