;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; anzu settings
;; --------------------------------------------------------------------
(use-package anzu
  :defer t
  :config
  (global-anzu-mode t)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))

;; --------------------------------------------------------------------
;; drag stuff
;; --------------------------------------------------------------------
(use-package drag-stuff
  :defer t
  :diminish drag-stuff-mode
  :config
  (drag-stuff-global-mode 1))

;; --------------------------------------------------------------------
;; undo tree
;; --------------------------------------------------------------------
(use-package undo-tree
  :config
  (setq-default undo-tree-visualizer-timestamps t
                undo-tree-auto-save-history nil  ; change undo history
                undo-tree-history-directory-alist `(("." . "~/.emacs.d/undo-tree")))  ; save all undo history into a single folder
  (global-undo-tree-mode))


;; --------------------------------------------------------------------
;; speed type
;; --------------------------------------------------------------------
(use-package speed-type
  :defer t)

;; --------------------------------------------------------------------
;; webpaste
;; --------------------------------------------------------------------
(use-package webpaste
  :defer t)

;; --------------------------------------------------------------------
;; darkroom
;; --------------------------------------------------------------------
(use-package darkroom
  :defer 1
  :config
  (setq darkroom-text-scale-increase 1.5))

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

;; --------------------------------------------------------------------
;; indent guide
;; --------------------------------------------------------------------
(use-package highlight-indentation)


(provide 'editing-assists-setup)
;;; editing-assists-setup ends here
