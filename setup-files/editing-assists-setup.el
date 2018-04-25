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
;; view large files
;; --------------------------------------------------------------------
(use-package speeddating
  :defer t)

;; --------------------------------------------------------------------
;; view large files
;; --------------------------------------------------------------------
(use-package vlf
  :defer 4
  :config
  (require 'vlf-setup))

;; --------------------------------------------------------------------
;; dictionary on point
;; --------------------------------------------------------------------
(use-package define-word
  :defer t)

(provide 'editing-assists-setup)
;;; editing-assists-setup ends here
