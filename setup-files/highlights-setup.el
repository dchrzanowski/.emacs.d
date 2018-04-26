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
(use-package highlight-thing
  :defer 2
  :diminish highlight-thing-mode
  :config
  (setq highlight-thing-case-sensitive-p t
        highlight-thing-exclude-thing-under-point nil
        highlight-thing-what-thing 'symbol
        highlight-thing-delay-seconds 0.1)
  (add-hook 'prog-mode-hook 'highlight-thing-mode))

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

;; --------------------------------------------------------------------
;; indent guide
;; --------------------------------------------------------------------
(use-package highlight-indentation)

;; --------------------------------------------------------------------
;; color identifiers
;; --------------------------------------------------------------------
(use-package color-identifiers-mode
  :defer 1
  :diminish color-identifiers-mode
  :config
  (setq color-identifiers-coloring-method 'sequential
        color-identifiers:num-colors '30
        color-identifiers:color-luminance 0.6
        color-identifiers:min-color-saturation 0.3
        color-identifiers:max-color-saturation 1.0)
  (global-color-identifiers-mode))



(provide 'highlights-setup)
;;; highlights-setup ends here
