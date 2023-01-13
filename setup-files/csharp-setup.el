;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; c#
;; --------------------------------------------------------------------
(use-package omnisharp
  :defer 3
  :config
  (defun my-csharp-mode-setup ()
    (c-set-style "ellemtel")
    (setq indent-tabs-mode nil
          c-syntactic-indentation t
          c-basic-offset 4
          truncate-lines t
          tab-width 4
          evil-shift-width 4)
    (local-set-key (kbd "C-c C-c") 'recompile))

  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t))

(provide 'csharp-setup)
;;; csharp-setup.el ends here
