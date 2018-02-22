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
   ;; default
   '(default ((t (:inherit nil :stipple nil :background "#282c34" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
   ;; auto highlight
   '(ahs-definition-face ((t (:background "black" :weight bold))))
   '(ahs-edit-mode-face ((t (:background "black" :foreground "dark orange"))))
   '(ahs-face ((t (:background "black"))))
   '(ahs-plugin-defalt-face ((t nil)))
   '(ahs-plugin-whole-buffer-face ((t (:foreground "orange" :underline t))))
   ;; anzu
   '(anzu-replace-highlight ((t (:background "black" :foreground "green"))))
   ;; avy
   '(avy-goto-char-timer-face ((t (:background "black" :foreground "green"))))
   '(avy-lead-face ((t (:background "black" :foreground "red"))))
   '(avy-lead-face-0 ((t (:background "black" :foreground "green"))))
   '(avy-lead-face-1 ((t (:background "black" :foreground "dark orange"))))
   '(avy-lead-face-2 ((t (:background "black" :foreground "white"))))
   ;; ace window
   '(aw-leading-char-face ((t (:background "black" :foreground "red"))))
   ;; cal
   '(cfw:face-toolbar-button-off ((t (:foreground "gray70"))))
   '(cfw:face-toolbar-button-on ((t (:foreground "white" :weight bold))))
   '(cfw:face-toolbar ((t (:foreground nil :background nil))))
   '(cfw:face-header ((t (:foreground "#4ea1e9" :background nil :weight bold))))
   '(cfw:face-saturday ((t (:foreground "#4ee979" :background nil :weight bold))))
   '(cfw:face-sunday ((t (:foreground "#dbe94e" :background nil :weight bold))))
   '(cfw:face-holiday ((t (:foreground "#e96c4e" :background "#080808"))))
   '(cfw:face-day-title ((t (:foreground "gray70" :background "#080808"))))
   '(cfw:face-grid ((t (:foreground "gray80" :background nil))))
   '(cfw:face-select ((t (:foreground "white" :background "#4ea1e9"))))
   '(cfw:face-today-title ((t (:foreground "white" :background "#D9534F"))))
   ;; company
   '(company-template-field ((t (:background "gray14" :foreground "SeaGreen4"))))
   ;; cursor
   '(cursor ((t (:background "#FF0000"))))
   ;; dired
   '(dired-directory ((t (:foreground "deep sky blue" :weight bold))))
   '(dired-header ((t (:height 110 :foreground "chartreuse3" :weight bold))))
   '(dired-flagged ((t (:foreground "red" :weight bold))))
   '(dired-marked ((t (:foreground "gold" :weight bold))))
   ;; evil
   '(evil-ex-lazy-highlight ((t (:background "black" :foreground "white"))))
   '(evil-ex-search ((t (:background "black" :foreground "white"))))
   '(evil-ex-substitute-matches ((t (:background "black" :foreground "white"))))
   ;; evil snipe
   '(evil-snipe-first-match-face ((t (:background "black" :foreground "green" :weight bold))))
   '(evil-snipe-matches-face ((t (:background "black" :foreground "green" :underline t :weight bold))))
   ;; eyebrowse
   '(eyebrowse-mode-line-active ((t (:foreground "chartreuse" :weight bold))))
   ;; font lock variable name
   '(font-lock-variable-name-face ((t (:foreground "#db5762"))))
   ;; helm
   '(helm-buffer-directory ((t (:foreground "deep sky blue" :weight bold))))
   '(helm-ff-directory ((t (:foreground "deep sky blue"))))
   '(helm-selection ((t (:inherit bold :background "#101318"))))
   ;; helm swoop
   '(helm-swoop-target-line-block-face ((t (:background "black" :foreground "chartreuse3"))))
   '(helm-swoop-target-line-face ((t (:background "#AAAAAA" :foreground "gray4" :inverse-video t))))
   '(helm-swoop-target-word-face ((t (:background "black" :foreground "chartreuse3"))))
   ;; highlight line
   '(hl-line ((t (:background "#20242A"))))
   ;; isearch
   '(isearch ((t (:background "black" :foreground "white"))))
   ;; lazy highlight
   '(lazy-highlight ((t (:background "black" :foreground "chartreuse3" :underline nil :weight normal))))
   ;; neotree
   '(neo-dir-link-face ((t (:foreground "#51afef" :weight bold))))
   ;; org agenda
   '(org-agenda-date ((t (:height 1.1))))
   '(org-agenda-date-today ((t (:height 1.2))))
   '(org-agenda-date-weekend ((t (:height 1.1))))
   ;; powerline
   '(powerline-active1 ((t (:inherit mode-line-emphasis :background "gray6" :foreground "light gray"))))
   ;; rainbow delimiters
   '(rainbow-delimiters-depth-1-face ((t (:foreground "LavenderBlush1"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "lime green"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "dark orange"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "red" :underline t))))
   ;; parens
   '(show-paren-match ((t (:background "black" :foreground "#ff6c6b" :weight normal))))
   '(show-paren-mismatch ((t (:background "black" :foreground "red" :underline t :weight normal))))
   ;; tabbar
   '(tabbar-selected ((t (:inherit tabbar-default :background "#21242b" :foreground "lime green" :weight bold))))
   '(tabbar-selected-modified ((t (:inherit tabbar-selected :foreground "lime green" :underline (:color foreground-color :style wave)))))
   '(tabbar-unselected ((t (:inherit tabbar-default :foreground "#9B9FA6"))))
   ;; tooltip
   '(tooltip ((t (:background "#080c14" :foreground "#bbc2cf"))))
   ;; webmode
   '(web-mode-html-tag-face ((t (:foreground "#db5762"))))))


(provide 'theme-setup)
;;; theme-setup ends here
