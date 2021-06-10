;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; load doom-modeline
;; --------------------------------------------------------------------
(use-package doom-modeline
  :ensure t
  :defer t
  :hook (after-init . doom-modeline-init)
  :config
  (setq doom-modeline-height 21
        doom-modeline-bar-width 6))

;; --------------------------------------------------------------------
;; load theme
;; --------------------------------------------------------------------
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; if the doom theme is updated then override the default color to #131519
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (custom-set-faces
   ;; default
   '(default ((t (:weight normal :height 108 :foundry "PfEd" :family "DejaVu Sans Mono"))))
   ;; anzu
   '(anzu-replace-highlight ((t (:background "black" :foreground "gold" :weight bold))))
   ;; avy
   '(avy-goto-char-timer-face ((t (:background "black" :foreground "gold" :weight bold))))
   '(avy-lead-face ((t (:background "black" :foreground "gold" :weight bold))))
   '(avy-lead-face-0 ((t (:background "black" :foreground "red" :weight bold))))
   '(avy-lead-face-1 ((t (:background "black" :foreground "chartreuse3" :weight bold))))
   '(avy-lead-face-2 ((t (:background "black" :foreground "white" :weight bold))))
   ;; ace window
   '(aw-leading-char-face ((t (:background "black" :foreground "red"))))
   ;; cal
   '(cfw:face-day-title ((t (:foreground "gray70" :background "#080808"))))
   '(cfw:face-grid ((t (:foreground "gray80" :background nil))))
   '(cfw:face-header ((t (:foreground "#4ea1e9" :background nil :weight bold))))
   '(cfw:face-holiday ((t (:foreground "#e96c4e" :background "#080808"))))
   '(cfw:face-saturday ((t (:foreground "#4ee979" :background nil :weight bold))))
   '(cfw:face-select ((t (:foreground "white" :background "#4ea1e9"))))
   '(cfw:face-sunday ((t (:foreground "#dbe94e" :background nil :weight bold))))
   '(cfw:face-today-title ((t (:foreground "white" :background "#D9534F"))))
   '(cfw:face-toolbar ((t (:foreground nil :background nil))))
   '(cfw:face-toolbar-button-off ((t (:foreground "gray70"))))
   '(cfw:face-toolbar-button-on ((t (:foreground "white" :weight bold))))
   ;; company
   '(company-template-field ((t (:background "gray14" :foreground "SeaGreen4"))))
   ;; cursor
   '(cursor ((t (:background "#FF0000"))))
   ;; dired
   '(dired-directory ((t (:foreground "deep sky blue" :weight bold))))
   '(dired-header ((t (:height 110 :foreground "chartreuse3" :weight bold))))
   '(dired-flagged ((t (:foreground "red" :weight bold))))
   '(dired-marked ((t (:foreground "gold" :inverse-video nil :weight bold))))
   ;; dired filter group
   '(dired-filter-group-header ((t (:background "black" :foreground "orange" :box (:line-width 1 :color "orange") :weight bold))))
   ;; eldoc-box
   '(eldoc-box-border ((t (:background "#51afef"))))
   ;; evil
   '(evil-ex-lazy-highlight ((t (:background "black" :foreground "white"))))
   '(evil-ex-search ((t (:background "black" :foreground "white"))))
   '(evil-ex-substitute-matches ((t (:background "black" :foreground "white"))))
   ;; evil snipe
   '(evil-snipe-first-match-face ((t (:background "black" :foreground "gold" :weight bold))))
   '(evil-snipe-matches-face ((t (:background "black" :foreground "gold" :weight bold))))
   ;; evil visual mark mode
   '(evil-visual-mark-face ((t (:background "black" :foreground "firebrick1" :box (:line-width 1 :color "firebrick1") :weight bold))))
   ;; eyebrowse
   '(eyebrowse-mode-line-active ((t (:foreground "chartreuse" :weight bold))))
   ;; flycheck-posframe
   '(flycheck-posframe-error-face ((t (:inherit flycheck-posframe-face :foreground "#ff6c6b" :box (:line-width 1 :color "#ff6c6b")))))
   '(flycheck-posframe-warning-face ((t (:inherit flycheck-posframe-face :foreground "#ECBE7B" :box (:line-width 1 :color "#ECBE7B")))))
   ;; flyspell
   '(flyspell-incorrect ((t (:underline (:color "chocolate" :style wave)))))
   ;; font lock variable name
   '(font-lock-variable-name-face ((t (:foreground "#db5762"))))
   '(font-lock-keyword-face ((t (:foreground "#51afef" :slant italic))))
   ;; helm
   '(helm-buffer-directory ((t (:foreground "deep sky blue" :weight bold))))
   '(helm-ff-directory ((t (:foreground "deep sky blue"))))
   '(helm-selection ((t (:inherit bold :background "#0b0c0e"))))
   ;; helm-posframe
   '(internal-border ((t (:background "#51afef"))))
   ;; helm swoop
   '(helm-swoop-target-line-block-face ((t (:background "black" :foreground "gold" :weight bold))))
   '(helm-swoop-target-line-face ((t (:background "#AAAAAA" :foreground "gray4" :inverse-video t))))
   '(helm-swoop-target-word-face ((t (:background "black" :foreground "gold" :weigth bold))))
   ;; highlight line
   '(hl-line ((t (:background "#0b0c0e"))))
   ;; highlight thing
   '(hi-yellow ((t (:background "nil" :underline "chartreuse" :weight bold))))
   '(highlight-thing ((t (:background "nil" :underline "chartreuse" :weight bold))))
   ;; isearch
   '(isearch ((t (:background "black" :foreground "white"))))
   ;; lazy highlight
   '(lazy-highlight ((t (:background "black" :foreground "gold" :underline nil :weight bold))))
   ;; mu4e
   '(mu4e-flagged-face ((t (:inherit font-lock-string-face :weight bold))))
   ;; neotree
   '(neo-dir-link-face ((t (:foreground "#51afef" :weight bold))))
   ;; org
   '(org-block ((t (:background "#181c24"))))
   '(org-block-begin-line ((t (:background "#181c24" :foreground "#5B6268"))))
   '(org-level-1 ((t (:height 1.2 :foreground "#51afef" :background nil))))
   '(org-level-2 ((t (:foreground "#a9a1e1" :background nil))))
   '(org-level-3 ((t (:foreground "#84b585" :background nil))))
   ;; org agenda
   '(org-agenda-date ((t (:background "#173874" :foreground "white" :box (:line-width 4 :color "#173874") :weight ultra-bold :height 1.0))))
   '(org-agenda-date-today ((t (:background "#276029" :foreground "white" :box (:line-width 4 :color "#276029") :weight ultra-bold :height 1.1))))
   '(org-agenda-date-weekend ((t (:background "#913351" :foreground "white" :box (:line-width 4 :color "#913351") :weight ultra-bold :height 1.0))))
   '(org-todo ((t (:foreground "#ff6655" :weight bold))))
   ;; powerline and modeline
   '(mode-line ((t (:background "gray2" :foreground "white" :box nil))))
   '(mode-line-inactive ((t (:background "grey11" :foreground "#5B6268" :box nil))))
   '(powerline-active1 ((t (:inherit mode-line-emphasis :background "gray4" :foreground "light gray"))))
   '(powerline-active2 ((t (:inherit mode-line :background "grey20" :foreground "white"))))
   '(powerline-evil-normal-face ((t (:background "dark green" :foreground "white"))))
   '(powerline-evil-operator-face ((t (:background "cyan" :foreground "black"))))
   '(powerline-inactive0 ((t (:inherit mode-line-inactive :background "grey11"))))
   '(powerline-inactive2 ((t (:inherit mode-line-inactive :background "grey11"))))
   ;; rainbow delimiters
   '(rainbow-delimiters-depth-1-face ((t (:foreground "LavenderBlush1"))))
   '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
   '(rainbow-delimiters-depth-6-face ((t (:foreground "lime green"))))
   '(rainbow-delimiters-depth-9-face ((t (:foreground "dark orange"))))
   '(rainbow-delimiters-unmatched-face ((t (:foreground "red" :underline t))))
   ;; region
   '(region ((t (:background "#32343a"))))
   ;; parens
   '(show-paren-match ((t (:background "black" :foreground "red" :weight normal))))
   '(show-paren-mismatch ((t (:background "black" :foreground "red" :underline t :weight normal))))
   ;; tab-line
   '(tab-line ((t (:background "#131519" :foreground "#131519" :box nil))))
   '(tab-line-close-highlight ((t (:foreground "#51afef"))))
   '(tab-line-tab ((t (:background "#131519" :foreground "lime green" :box (:line-width (6 . 4) :color "#131519")))))
   '(tab-line-tab-current ((t (:background "#23272e" :foreground "lime green" :box (:line-width (6 . 4) :color "#131519" :style flat-button)))))
   '(tab-line-tab-inactive ((t (:inherit tab-line-tab :background "#131519" :foreground "#6a727a" :box (:line-width (1 . 4) :color "#131519")))))
   ;; tooltip
   '(tooltip ((t (:background "#080c14" :foreground "#bbc2cf"))))
   ;; window border
   '(vertical-border ((t (:background "gray13" :foreground "gray13"))))
   ;; webmode
   '(web-mode-html-tag-face ((t (:foreground "#db5762"))))))


(provide 'theme-setup)
;;; theme-setup ends here
