;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; hl-todo
;; --------------------------------------------------------------------
(use-package hl-todo
  :diminish global-hl-todo-mode
  :config
  (global-hl-todo-mode)
  (add-hook 'prog-mode 'hl-todo-mode))  ;; just in case

;; --------------------------------------------------------------------
;; auto highlight mode
;; --------------------------------------------------------------------
(use-package auto-highlight-symbol
  :diminish auto-highlight-symbol-mode
  :config
  (global-auto-highlight-symbol-mode t)
  (add-hook 'prog-mode 'auto-highlight-symbol-mode))

;; --------------------------------------------------------------------
;; rainbow delimiters
;; --------------------------------------------------------------------
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; --------------------------------------------------------------------
;; rainbow-mode
;; --------------------------------------------------------------------
(use-package rainbow-mode
  :defer t)

(provide 'highlights-setup)
;;; highlights-setup ends here
