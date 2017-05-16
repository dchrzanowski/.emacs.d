;;; package --- Summary
;;; Commentary:
;;; Code:
;; =================================================================================================================
;; custom set variables
;; =================================================================================================================
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(anzu-replace-to-string-separator " > ")
 '(async-shell-command-buffer (quote confirm-new-buffer))
 '(before-save-hook (quote (delete-trailing-whitespace)))
 '(bmkp-last-as-first-bookmark-file "/home/grimscythe/.emacs.d/bookmarks")
 '(browse-url-browser-function (quote browse-url-chromium))
 '(column-number-mode t)
 '(company-auto-complete-chars (quote (32 41 46)))
 '(company-backends
   (quote
    (company-emacs-eclim company-bbdb company-css company-semantic company-clang company-xcode company-cmake company-capf company-files
                         (company-dabbrev-code company-gtags company-etags company-keywords company-web-html)
                         company-oddmuse)))
 '(company-begin-commands
   (quote
    (self-insert-command org-self-insert-command orgtbl-self-insert-command c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash)))
 '(company-dabbrev-code-everywhere t)
 '(company-dabbrev-code-modes
   (quote
    (prog-mode batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode lua-mode python-mode web-mode js2-mode java-mode)))
 '(company-dabbrev-code-other-buffers (quote all))
 '(company-dabbrev-downcase nil)
 '(company-frontends
   (quote
    (company-pseudo-tooltip-unless-just-one-frontend company-echo-metadata-frontend company-preview-if-just-one-frontend company-quickhelp-frontend)))
 '(company-gtags-modes (quote (prog-mode jde-mode python-mode)))
 '(company-idle-delay 0.5)
 '(company-minimum-prefix-length 2)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-minimum-width 40)
 '(cua-rectangle-mark-key [C-M-return])
 '(cursor-type (quote bar))
 '(custom-safe-themes
   (quote
    ("a0dc0c1805398db495ecda1994c744ad1a91a9455f2a17b59b716f72d3585dde" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" "6254372d3ffe543979f21c4a4179cd819b808e5dd0f1787e2a2a647f5759c1d1" "1160f5fc215738551fce39a67b2bcf312ed07ef3568d15d53c87baa4fd1f4d4e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "02e30200f168b362d5e12cdef4afb2453aa9e00af11cc93c2bcd2a413251eac1" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "54ece5659cc7acdcd529dddd78675c2972a5ac69260af4a6aec517dcea16208b" "770181eda0f652ef9293e8db103a7e5ca629c516ca33dfa4709e2c8a0e7120f3" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" default)))
 '(default-frame-alist (quote ((vertical-scroll-bars))))
 '(eclimd-default-workspace "~/Eclipse")
 '(expand-region-preferred-python-mode (quote fgallina-python))
 '(expand-region-smart-cursor t)
 '(fci-rule-color "#3E4451")
 '(flycheck-flake8-maximum-line-length 120)
 '(flycheck-idle-change-delay 5)
 '(flycheck-python-flake8-executable nil)
 '(ggtags-executable-directory "/usr/bin")
 '(ggtags-use-project-gtagsconf nil)
 '(global-flycheck-mode t)
 '(global-semantic-decoration-mode nil)
 '(global-semantic-idle-scheduler-mode t)
 '(global-semanticdb-minor-mode t)
 '(helm-external-programs-associations (quote (("html" . "chromium"))))
 '(helm-swoop-split-with-multiple-windows t)
 '(line-move-visual nil)
 '(mc/always-run-for-all t)
 '(menu-bar-mode nil)
 '(org-from-is-user-regexp nil)
 '(org-log-done t)
 '(org-src-fontify-natively t)
 '(org-support-shift-select t)
 '(package-selected-packages
   (quote
    (paradox restclient realgud evil-nerd-commenter eyebrowse emmet-mode emmet ox-reveal ox-impress-js org-impress-js ox-gfm org-brain company-ispell atom-one-dark-theme beacon hl-todo imenu-anywhere pomidor company-yasnippet eclimd evil-lion git-gutter-fringe git-timemachine lice dired-launch palette auto-yasnippet ace-jump-helm-line quickrun flx evil-magit nlinum nlinum-relative evil-god-state god-mode evil-args evil-visualstar evil-matchit evil-anzu evil-surround evil-leader powerline-evil powerline htmlize latex-extra tabbar rainbow-mode rainbow-delimiters org-bullets helm-swoop company-web company-jedi auto-package-update auto-highlight-symbol anaphora ac-html-bootstrap ac-html ac-dabbrev undo-tree)))
 '(palette-hex-rgb-digits 2)
 '(paradox-github-token t)
 '(powerline-gui-use-vcs-glyph nil)
 '(python-shell-completion-native-disabled-interpreters (quote ("pypy" "python")))
 '(save-place t nil (saveplace))
 '(scroll-preserve-screen-position 1)
 '(send-mail-function (quote mailclient-send-it))
 '(shell-pop-full-span t)
 '(shell-pop-shell-type
   (quote
    ("eshell" "*eshell*"
     (lambda nil
       (eshell shell-pop-term-shell)))))
 '(shell-pop-term-shell "/bin/bash")
 '(shell-pop-universal-key "C-`")
 '(shell-pop-window-position "bottom")
 '(shell-pop-window-size 30)
 '(show-paren-mode t)
 '(split-height-threshold 5)
 '(split-width-threshold 5)
 '(sr-show-file-attributes nil)
 '(sr-traditional-other-window t)
 '(sr-windows-default-ratio 80)
 '(tabbar-background-color "#2F343F")
 '(tabbar-mode t nil (tabbar))
 '(tabbar-mwheel-mode t nil (tabbar))
 '(tabbar-separator (quote (1)))
 '(tool-bar-mode nil)
 '(track-eol t)
 '(web-mode-auto-close-style 2)
 '(web-mode-enable-current-column-highlight t)
 '(web-mode-enable-current-element-highlight t)
 '(which-key-hide-alt-key-translations nil)
 '(which-key-mode t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#282C34" :foreground "#ABB2BF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(ahs-definition-face ((t (:background "gray8" :weight bold))))
 '(ahs-edit-mode-face ((t (:background "dark slate gray" :foreground "White"))))
 '(ahs-face ((t (:background "gray8" :foreground "dark gray"))))
 '(ahs-plugin-defalt-face ((t nil)))
 '(ahs-plugin-whole-buffer-face ((t (:foreground "orange" :underline t))))
 '(anzu-replace-highlight ((t (:background "black" :foreground "dark orange"))))
 '(avy-lead-face ((t (:background "black" :foreground "red"))))
 '(avy-lead-face-0 ((t (:background "black" :foreground "green"))))
 '(avy-lead-face-1 ((t (:background "black" :foreground "dark orange"))))
 '(avy-lead-face-2 ((t (:background "black" :foreground "white"))))
 '(company-template-field ((t (:background "gray14" :foreground "SeaGreen4"))))
 '(cursor ((t (:background "#FF0000"))))
 '(diredp-dir-heading ((t (:foreground "white" :underline t :weight bold :height 120))))
 '(diredp-dir-name ((t (:background "RoyalBlue4" :foreground "white" :weight bold))))
 '(diredp-file-name ((t (:foreground "white"))))
 '(evil-ex-lazy-highlight ((t (:background "black" :foreground "white"))))
 '(evil-ex-search ((t (:background "black" :foreground "white"))))
 '(evil-ex-substitute-matches ((t (:background "black" :foreground "white"))))
 '(evil-snipe-first-match-face ((t (:background "black" :foreground "red" :weight bold))))
 '(evil-snipe-matches-face ((t (:background "black" :foreground "red"))))
 '(font-lock-variable-name-face ((t (:foreground "#db5762"))))
 '(helm-swoop-target-line-block-face ((t (:background "gray21" :foreground "#AAAAAA"))))
 '(helm-swoop-target-line-face ((t (:background "gray21" :foreground "#AAAAAA"))))
 '(helm-swoop-target-word-face ((t (:background "gray13" :foreground "violet red"))))
 '(isearch ((t (:background "black" :foreground "white"))))
 '(lazy-highlight ((t (:foreground "chartreuse3" :underline nil))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "LavenderBlush1"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red" :underline t))))
 '(sr-active-path-face ((t (:background "black" :foreground "white" :weight bold :height 120))))
 '(sr-editing-path-face ((t (:background "red" :foreground "white" :weight bold :height 120))))
 '(sr-highlight-path-face ((t (:background "yellow" :foreground "black" :weight bold :height 120))))
 '(sr-passive-path-face ((t (:background "white" :foreground "black" :weight bold :height 120))))
 '(tabbar-button ((t (:inherit tabbar-default))))
 '(tabbar-default ((t (:inherit variable-pitch :background "#2F343F" :foreground "white smoke" :height 0.8))))
 '(tabbar-highlight ((t (:foreground "orange red"))))
 '(tabbar-modified ((t (:inherit tabbar-default :foreground "green"))))
 '(tabbar-selected ((t (:inherit tabbar-default :foreground "olive drab" :underline t))))
 '(tabbar-selected-modified ((t (:inherit tabbar-default :foreground "green" :underline (:color foreground-color :style wave)))))
 '(tabbar-separator ((t (:inherit tabbar-default))))
 '(tabbar-unselected ((t (:inherit tabbar-default))))
 '(web-mode-html-tag-face ((t (:foreground "#db5762")))))
(provide 'custom)
;;; custom.el ends here
