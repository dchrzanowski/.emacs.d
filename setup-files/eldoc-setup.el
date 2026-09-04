;;; package -- Summary  -*- lexical-binding: t; -*-
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; eldoc
;; --------------------------------------------------------------------
(global-eldoc-mode)
(setq eldoc-idle-delay 0.2)
(setq eldoc-help-at-pt t)

;; (use-package eldoc-box
;;   :config
;;   (setq x-gtk-resize-child-frames 'hide)
;;   (add-hook 'prog-mode-hook 'eldoc-box-hover-mode))

(provide 'eldoc-setup)
;;; eldoc-setup.el ends here
