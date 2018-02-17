;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; load powerline
;; --------------------------------------------------------------------
(use-package powerline
  :config
  (use-package powerline-evil
    :config
    (powerline-evil-center-color-theme)
    (setq-default powerline-default-separator (quote wave))))

;; --------------------------------------------------------------------
;; load theme
;; --------------------------------------------------------------------
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (custom-set-faces
   '(tabbar-selected ((t (:inherit tabbar-default :background "#21242b" :foreground "lime green" :weight bold))))
   '(tabbar-selected-modified ((t (:inherit tabbar-selected :foreground "lime green" :underline (:color foreground-color :style wave)))))
   '(tabbar-unselected ((t (:inherit tabbar-default :foreground "#9B9FA6"))))
   '(default ((t (:inherit nil :stipple nil :background "#282c34" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
   '(org-agenda-date ((t (:height 1.1))))
   '(org-agenda-date-today ((t (:height 1.2))))
   '(org-agenda-date-weekend ((t (:height 1.1))))))


(provide 'theme-setup)
;;; theme-setup ends here
