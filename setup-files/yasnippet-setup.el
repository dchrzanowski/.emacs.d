;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; yasnippets and autoyasnippet
;; --------------------------------------------------------------------
(use-package yasnippet
  :defer t
  :config
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change))
  (yas-global-mode 1))

(use-package auto-yasnippet)

(provide 'yasnippet-setup)
;;; yasnippet-setup.el ends here
