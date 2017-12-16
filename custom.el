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
    (company-tide company-emacs-eclim company-bbdb company-css company-semantic company-clang company-xcode company-cmake company-capf company-files
                  (company-dabbrev-code company-gtags company-etags company-keywords company-web-html)
                  company-oddmuse company-tern)))
 '(company-begin-commands
   (quote
    (self-insert-command org-self-insert-command orgtbl-self-insert-command c-scope-operator c-electric-colon c-electric-lt-gt c-electric-slash)))
 '(company-dabbrev-code-everywhere t)
 '(company-dabbrev-code-modes
   (quote
    (prog-mode batch-file-mode csharp-mode css-mode erlang-mode haskell-mode jde-mode lua-mode python-mode web-mode js2-mode java-mode typescript-mode)))
 '(company-dabbrev-code-other-buffers (quote all))
 '(company-dabbrev-downcase nil)
 '(company-frontends
   (quote
    (company-pseudo-tooltip-unless-just-one-frontend company-echo-metadata-frontend company-preview-if-just-one-frontend company-quickhelp-frontend)))
 '(company-gtags-modes (quote (prog-mode jde-mode python-mode)))
 '(company-idle-delay 0.2)
 '(company-minimum-prefix-length 2)
 '(company-show-numbers t)
 '(company-tooltip-align-annotations t)
 '(company-tooltip-minimum-width 40)
 '(cua-rectangle-mark-key [C-M-return])
 '(cursor-type (quote bar))
 '(custom-safe-themes
   (quote
    ("63b822ccd7a1928a7cbc88037dddf7b74b2f8a507e1bccd7281f20646f72cd0a" "6bde11b304427c7821b72a06a60e8d079b8f7ae10b407d8af37ed5e5d59b1324" "e91ca866d6cbb79786e314e0466f4f1b8892b72e77ed702e53bf7565e0dfd469" "9f3181dc1fabe5d58bbbda8c48ef7ece59b01bed606cfb868dd147e8b36af97c" "227e2c160b0df776257e1411de60a9a181f890cfdf9c1f45535fc83c9b34406b" "a0dc0c1805398db495ecda1994c744ad1a91a9455f2a17b59b716f72d3585dde" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" "6254372d3ffe543979f21c4a4179cd819b808e5dd0f1787e2a2a647f5759c1d1" "1160f5fc215738551fce39a67b2bcf312ed07ef3568d15d53c87baa4fd1f4d4e" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "02e30200f168b362d5e12cdef4afb2453aa9e00af11cc93c2bcd2a413251eac1" "71ecffba18621354a1be303687f33b84788e13f40141580fa81e7840752d31bf" "54ece5659cc7acdcd529dddd78675c2972a5ac69260af4a6aec517dcea16208b" "770181eda0f652ef9293e8db103a7e5ca629c516ca33dfa4709e2c8a0e7120f3" "12b4427ae6e0eef8b870b450e59e75122d5080016a9061c9696959e50d578057" default)))
 '(default-frame-alist (quote ((vertical-scroll-bars))))
 '(dired-du-size-format t)
 '(eclimd-default-workspace "~/eclipse-workspace")
 '(expand-region-preferred-python-mode (quote fgallina-python))
 '(expand-region-smart-cursor t)
 '(fci-rule-color "#3E4451")
 '(flycheck-flake8-maximum-line-length 120)
 '(flycheck-idle-change-delay 5)
 '(flycheck-python-flake8-executable nil)
 '(ggtags-executable-directory "/usr/bin")
 '(ggtags-use-project-gtagsconf nil)
 '(git-messenger:show-detail t)
 '(git-messenger:use-magit-popup t)
 '(global-flycheck-mode t)
 '(global-semantic-decoration-mode nil)
 '(global-semantic-idle-scheduler-mode t)
 '(global-semanticdb-minor-mode t)
 '(helm-external-programs-associations (quote (("html" . "chromium"))))
 '(helm-swoop-split-with-multiple-windows t)
 '(line-move-visual nil)
 '(mc/always-run-for-all t)
 '(menu-bar-mode nil)
 '(message-directory "~/Mail/maestrosartori69/[Gmail].Sent Mail")
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "s")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "trash" :query "tag:trash" :key "b")
     (:name "stash" :query "tag:stash" :key "t"))))
 '(org-agenda-files
   (quote
    ("/home/grimscythe/Google Drive/org/notes.org" "/home/grimscythe/Google Drive/org/projects/myLectures/myLectures.org" "/home/grimscythe/Google Drive/org/projects/myLectures/myLecturesScalePlan.org" "/home/grimscythe/Google Drive/org/Calendar.org" "/home/grimscythe/Google Drive/org/refile.org")))
 '(org-from-is-user-regexp nil)
 '(org-log-done t)
 '(org-src-fontify-natively t)
 '(org-support-shift-select t)
 '(package-selected-packages
   (quote
    (exato evil-indent-plus evil-ediff evil-numbers evil-number evil-exchange ace-link git-messenger omnisharp omnisharp-emacs markdown-mode notmuch js-doc zenity-color-picker dired-du evil-snipe helm-dash company-tern web-beautify which-key doom-themes paradox restclient realgud evil-nerd-commenter eyebrowse emmet-mode emmet ox-reveal ox-impress-js org-impress-js ox-gfm org-brain company-ispell atom-one-dark-theme hl-todo imenu-anywhere pomidor company-yasnippet eclimd evil-lion git-gutter-fringe git-timemachine lice dired-launch auto-yasnippet quickrun flx evil-magit nlinum nlinum-relative evil-god-state god-mode evil-args evil-visualstar evil-matchit evil-anzu evil-surround evil-leader powerline-evil powerline htmlize latex-extra tabbar rainbow-mode rainbow-delimiters org-bullets helm-swoop company-web company-jedi auto-package-update auto-highlight-symbol anaphora ac-html-bootstrap ac-dabbrev undo-tree)))
 '(palette-hex-rgb-digits 2)
 '(paradox-github-token t)
 '(powerline-gui-use-vcs-glyph nil)
 '(python-shell-completion-native-disabled-interpreters (quote ("pypy" "python")))
 '(save-place-mode t)
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
 '(default ((t (:inherit nil :stipple nil :background "#282c34" :foreground "#bbc2cf" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 90 :width normal :foundry "PfEd" :family "DejaVu Sans Mono"))))
 '(ahs-definition-face ((t (:background "black" :weight bold))))
 '(ahs-edit-mode-face ((t (:background "black" :foreground "dark orange"))))
 '(ahs-face ((t (:background "black"))))
 '(ahs-plugin-defalt-face ((t nil)))
 '(ahs-plugin-whole-buffer-face ((t (:foreground "orange" :underline t))))
 '(anzu-replace-highlight ((t (:background "black" :foreground "green"))))
 '(avy-goto-char-timer-face ((t (:background "black" :foreground "green"))))
 '(avy-lead-face ((t (:background "black" :foreground "red"))))
 '(avy-lead-face-0 ((t (:background "black" :foreground "green"))))
 '(avy-lead-face-1 ((t (:background "black" :foreground "dark orange"))))
 '(avy-lead-face-2 ((t (:background "black" :foreground "white"))))
 '(aw-leading-char-face ((t (:background "black" :foreground "red"))))
 '(company-template-field ((t (:background "gray14" :foreground "SeaGreen4"))))
 '(cursor ((t (:background "#FF0000"))))
 '(diredp-dir-heading ((t (:foreground "white" :underline t :weight bold :height 120))))
 '(diredp-dir-name ((t (:background "RoyalBlue4" :foreground "white" :weight bold))))
 '(diredp-file-name ((t (:foreground "white"))))
 '(evil-ex-lazy-highlight ((t (:background "black" :foreground "white"))))
 '(evil-ex-search ((t (:background "black" :foreground "white"))))
 '(evil-ex-substitute-matches ((t (:background "black" :foreground "white"))))
 '(evil-goggles-default-face ((t (:inherit (quote bmkp-no-local)))))
 '(evil-goggles-delete-face ((t (:inherit (quote bmkp-su-or-sudo)))))
 '(evil-goggles-paste-face ((t (:inherit (quote bmkp-sequence)))))
 '(evil-goggles-yank-face ((t (:inherit (quote bmkp-non-file)))))
 '(evil-snipe-first-match-face ((t (:background "black" :foreground "green" :weight bold))))
 '(evil-snipe-matches-face ((t (:background "black" :foreground "green" :underline t :weight bold))))
 '(eyebrowse-mode-line-active ((t (:foreground "chartreuse" :weight bold))))
 '(font-lock-variable-name-face ((t (:foreground "#db5762"))))
 '(helm-buffer-directory ((t (:foreground "deep sky blue" :weight bold))))
 '(helm-ff-directory ((t (:foreground "deep sky blue"))))
 '(helm-selection ((t (:inherit bold :background "#101318"))))
 '(helm-swoop-target-line-block-face ((t (:background "black" :foreground "chartreuse3"))))
 '(helm-swoop-target-line-face ((t (:background "#AAAAAA" :foreground "gray4" :inverse-video t))))
 '(helm-swoop-target-word-face ((t (:background "black" :foreground "chartreuse3"))))
 '(hl-line ((t (:background "#20242A"))))
 '(isearch ((t (:background "black" :foreground "white"))))
 '(lazy-highlight ((t (:background "black" :foreground "chartreuse3" :underline nil :weight normal))))
 '(neo-dir-link-face ((t (:foreground "#51afef" :weight bold))))
 '(powerline-active1 ((t (:inherit mode-line-emphasis :background "gray6" :foreground "light gray"))))
 '(rainbow-delimiters-depth-1-face ((t (:foreground "LavenderBlush1"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "deep sky blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "lime green"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "dark orange"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red" :underline t))))
 '(show-paren-match ((t (:background "black" :foreground "#ff6c6b" :weight normal))))
 '(show-paren-mismatch ((t (:background "black" :foreground "red" :underline t :weight normal))))
 '(tabbar-selected ((t (:inherit tabbar-default :background "#21242b" :foreground "lime green" :weight bold))))
 '(tabbar-selected-modified ((t (:inherit tabbar-selected :foreground "lime green" :underline (:color foreground-color :style wave)))))
 '(tabbar-unselected ((t (:inherit tabbar-default :foreground "#9B9FA6"))))
 '(tooltip ((t (:background "#080c14" :foreground "#bbc2cf"))))
 '(web-mode-html-tag-face ((t (:foreground "#db5762")))))
(provide 'custom)
;;; custom.el ends here
