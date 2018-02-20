;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; general leader
;; --------------------------------------------------------------------
(general-define-key :states 'motion
                    "SPC" 'nil)

(general-define-key
 :states '(normal visual motion emacs)
 :prefix "SPC"
 ;; show filename
 "`" 'show-file-name
 ;; helm
 "x" 'helm-M-x
 "w" 'helm-mini
 "f" 'helm-swoop-without-pre-input
 "F" 'helm-find-files
 "M-f" 'helm-multi-swoop-all
 "b" 'helm-filtered-bookmarks
 "m" 'helm-all-mark-rings
 "r" 'helm-semantic-or-imenu
 "R" 'helm-imenu-anywhere
 "y" 'helm-show-kill-ring
 "Y" 'helm-register
 "hg" 'helm-do-grep-ag
 "hG" 'helm-google-suggest
 "ha" 'helm-do-ag
 "hl" 'helm-locate
 "hw" 'helm-do-ag-buffers
 "hd" 'helm-dash
 "hs" 'helm-system-packages
 ;; help
 "hv" 'describe-variable
 "hf" 'describe-function
 "hm" 'describe-mode
 "hk" 'describe-key
 ;; projectile
 "pp" 'helm-projectile-switch-project
 "pf" 'helm-projectile-find-file
 "pF" 'helm-projectile-find-file-in-known-projects
 "pi" 'projectile-invalidate-cache
 "pk" 'projectile-kill-buffers
 "pa" 'helm-projectile-ag
 ;; save all buffers
 "s" (lambda() (interactive) (save-some-buffers t))
 ;; hydras
 "t" 'hydra-rare-launcher/body
 "d" 'hydra-window-operations/body
 "I" 'hydra-indent/body
 "gc" 'hydra-smerge/body
 ;; dired
 "D" 'dired
 ;; winner
 "q" 'winner-undo
 "Q" 'winner-redo
 "M-q" 'clean-buffer-list
 ;; avy
 "e" 'avy-goto-char-timer
 "E" 'evil-avy-goto-char-2
 "j" 'evil-avy-goto-word-or-subword-1
 "J" 'evil-avy-goto-word-0
 ;; anzu
 "a" 'anzu-query-replace
 "A" 'anzu-query-replace-at-cursor
 ;; evil comment
 ";" 'evilnc-comment-or-uncomment-lines
 "kk" 'evilnc-comment-or-uncomment-lines
 "kl" 'evilnc-quick-comment-or-uncomment-to-the-line
 "kc" 'evilnc-copy-and-comment-lines
 "kp" 'evilnc-comment-or-uncomment-paragraphs
 "kr" 'comment-or-uncomment-region
 "kv" 'evilnc-toggle-invert-comment-line-by-line
 ;; whitespace mode
 "W" 'whitespace-mode
 ;; editing assists
 "i" 'cleanup-buffer
 ;; dumb jump
 "lo" 'dumb-jump-go-other-window
 "lj" 'dumb-jump-go
 "lx" 'dumb-jump-go-prefer-external
 "lz" 'dumb-jump-go-prefer-external-other-window
 "lq" 'dumb-jump-quick-look
 ;; git
 "gs" 'magit-status
 "gi" 'magit-init
 "gl" 'magit-log-popup
 "gr" 'magit-list-repositories
 "gm" 'git-messenger:popup-message
 "gM" 'git-messenger:popup-show-verbose
 ;; org
 "oc" 'org-capture
 "ol" 'org-store-link
 "oA" 'org-agenda-list
 "oa" 'org-agenda-list-and-delete-other-windows
 "ob" 'org-iswitchb
 "ok" 'cfw:open-org-calendar
 "oK" 'calendar
 ;; eyebrowse
 "'" 'eyebrowse-last-window-config
 "0" 'eyebrowse-switch-to-window-config-0
 "1" 'eyebrowse-switch-to-window-config-1
 "2" 'eyebrowse-switch-to-window-config-2
 "3" 'eyebrowse-switch-to-window-config-3
 "4" 'eyebrowse-switch-to-window-config-4
 "5" 'eyebrowse-switch-to-window-config-5
 "6" 'eyebrowse-switch-to-window-config-6
 "7" 'eyebrowse-switch-to-window-config-7
 "8" 'eyebrowse-switch-to-window-config-8
 "9" 'eyebrowse-switch-to-window-config-9)

;; --------------------------------------------------------------------
;; web mode leader
;; --------------------------------------------------------------------
(general-define-key
 :states '(normal visual emacs)
 :prefix "SPC"
 :keymaps 'web-mode-map
 "cew" 'web-mode-element-wrap
 "ces" 'web-mode-element-select
 "cea" 'web-mode-element-content-select
 "cek" 'web-mode-element-kill
 "cei" 'web-mode-element-insert
 "cec" 'web-mode-element-clone
 "cer" 'web-mode-element-rename
 "cet" 'web-mode-element-transpose
 "cen" 'web-mode-element-next
 "cep" 'web-mode-element-previous
 "cai" 'web-mode-attribute-insert
 "cak" 'web-mode-attribute-kill
 "can" 'web-mode-attribute-next
 "cap" 'web-mode-attribute-previous
 "cas" 'web-mode-attribute-select
 "ctm" 'web-mode-tag-match
 "cts" 'web-mode-tag-select
 "ctn" 'web-mode-tag-next
 "ctp" 'web-mode-tag-previous
 "cdn" 'web-mode-dom-normalize
 "cdt" 'web-mode-dom-traverse)

;; --------------------------------------------------------------------
;; org mode leader
;; --------------------------------------------------------------------
(general-define-key
 :states '(normal visual emacs)
 :prefix "SPC"
 :keymaps 'org-mode-map
 "cf" 'helm-org-rifle
 "cs" 'org-download-screenshot
 "cy" 'org-download-yank)

;; --------------------------------------------------------------------
;; org agenda leader
;; --------------------------------------------------------------------
(general-define-key
 :states '(normal visual emacs)
 :prefix "SPC"
 :keymaps 'org-agenda-mode-map
 "oa" 'hydra-org-agenda/body)

;; --------------------------------------------------------------------
;; js2 leader
;; --------------------------------------------------------------------
(general-define-key
 :states '(normal visual emacs)
 :prefix "SPC"
 :keymaps 'js2-mode-map
 "ci" 'js-doc-insert-function-doc-snippet
 "cc" 'tern-get-type
 "cr" 'tern-rename-variable
 "c." 'tern-find-definition
 "c," 'tern-pop-find-definition
 "cd" 'tern-get-docs)

;; --------------------------------------------------------------------
;; typescript leader
;; --------------------------------------------------------------------
(general-define-key
 :states '(normal visual emacs)
 :prefix "SPC"
 :keymaps 'typescript-mode-map
 "c" 'tide-documentation-at-point)

;; --------------------------------------------------------------------
;; pomidor leader
;; --------------------------------------------------------------------
(general-define-key
 :states '(normal visual emacs)
 :prefix "SPC"
 :keymaps 'pomidor-mode-map
 "SPC" 'pomidor-break)

;; --------------------------------------------------------------------
;; neotree leader
;; --------------------------------------------------------------------
(general-define-key
 :states '(normal visual emacs)
 :prefix "SPC"
 :keymaps 'neotree-mode-map
 "r" 'neotree-rename-node
 "D" 'neotree-delete-node
 "n" 'neotree-create-node
 "c" 'neotree-copy-node)

(provide 'key-leaders-setup)
;;; key-leaders-setup.el ends here
