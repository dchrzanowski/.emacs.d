;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; key binds / shortcuts
;; --------------------------------------------------------------------

;; minibuffer allow to paste in the Evil register way
(define-key minibuffer-local-map (kbd "C-r") 'evil-paste-from-register)

;; neotree
;; (global-set-key [f8] 'neotree-toggle)
(define-key undo-tree-map (kbd "C-/") 'nil)
(global-set-key (kbd "C-/") 'neotree-toggle)
(defun neotree-enter-and-close-neotree ()
  "Open a file under point and close neotree."
  (interactive)
  (neotree-enter)
  (neotree-hide))

(evil-define-key 'normal neotree-mode-map (kbd "L") 'neotree-enter-and-close-neotree)
(evil-define-key 'normal neotree-mode-map (kbd "l") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "h") 'neotree-select-up-node)
(evil-define-key 'normal neotree-mode-map (kbd "o") 'neotree-hidden-file-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-change-root)
(evil-define-key 'normal neotree-mode-map (kbd "J") 'neotree-open-xdg-on-point)
(evil-define-key 'normal neotree-mode-map (kbd "d") 'neotree-enter-ace-window)

;; additional evil leader bindings for neotree
(evil-leader/set-key-for-mode 'neotree-mode
  "r" 'neotree-rename-node
  "D" 'neotree-delete-node
  "n" 'neotree-create-node
  "c" 'neotree-copy-node)

;; sunrise
(global-set-key [f9] 'sunrise)

;; quickrun
(global-set-key [f5] 'quickrun)

;; pomidor
(global-set-key [f7] 'pomidor)

;; smartparens
(define-key global-map (kbd "M-S") 'sp-split-sexp)

;; arc-mode
;;(define-key archive-mode-map (kbd "F") #'archive-extract-marked-to-file)

;; packages
(global-set-key (kbd "M-[") 'package-install)
(global-set-key (kbd "M-]") 'paradox-list-packages)

;; upcase/downcase
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)

;; open init with F6
(global-set-key (kbd "<f6>") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))

;;anzu binds
(global-set-key (kbd "C-f") 'anzu-query-replace)
(global-set-key (kbd "C-M-r") 'anzu-query-replace-at-cursor-thing)
(global-set-key (kbd "C-M-s-r") 'anzu-query-replace-at-cursor)

;; webpaste
(global-set-key (kbd "C-c C-p C-b") 'webpaste-paste-buffer)
(global-set-key (kbd "C-c C-p C-r") 'webpaste-paste-region)

;;rainbow mode
(global-set-key (kbd "C-c r") 'rainbow-mode)

;; ace window
(global-set-key (kbd "M-d") 'ace-window)
(define-key dired-mode-map (kbd "M-d") 'ace-window)

;; changed recenter
(define-key global-map (kbd "C-l") 'recenter)

;; helm bindings
(global-set-key (kbd "M-x") 'helm-M-x)
(with-eval-after-load 'magit-mode
  (define-key magit-mode-map (kbd "M-x") 'helm-M-x))

(global-set-key (kbd "M-w") 'helm-buffers-list)

;; helm-swoop binds
(global-set-key (kbd "M-f") 'helm-swoop)
(global-set-key (kbd "M-F") 'helm-multi-swoop-all)

;; avy
(global-set-key (kbd "M-e") 'avy-goto-char-timer)
(global-set-key (kbd "M-j") 'avy-goto-word-or-subword-1)

;; helm bookmark
(global-set-key (kbd "C-c b") 'helm-bookmarks)
(global-set-key (kbd "C-c C-b") 'helm-bookmarks)

;; vim like helm movement
(define-key helm-map (kbd "M-j") 'helm-next-line)
(define-key helm-map (kbd "M-k") 'helm-previous-line)
(define-key helm-map (kbd "C-j") 'helm-next-page)
(define-key helm-map (kbd "C-k") 'helm-previous-page)
(define-key helm-map (kbd "M-h") 'helm-next-source)
(define-key helm-map (kbd "C-S-h") 'describe-key)
(define-key helm-map (kbd "M-l") (kbd "RET"))
(define-key helm-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-map (kbd "M-i") 'helm-select-action)
(define-key helm-map [tab] 'helm-execute-persistent-action)
(define-key helm-map [escape] 'helm-keyboard-quit)
(dolist (keymap (list helm-find-files-map helm-read-file-map helm-generic-files-map))
  (define-key keymap (kbd "M-l") 'helm-execute-persistent-action)
  (define-key keymap (kbd "M-h") 'helm-find-files-up-one-level)
  (define-key keymap (kbd "M-J") 'helm-ff-run-open-file-with-default-tool)
  (define-key keymap (kbd "M-K") 'helm-ff-open-dired-at-point)
  (define-key keymap (kbd "M-i") 'helm-select-action)
  (define-key keymap (kbd "C-S-h") 'describe-key)
  (define-key keymap (kbd "TAB") 'helm-execute-persistent-action)
  (define-key keymap (kbd "C-r") 'evil-paste-from-register)
  (define-key keymap [tab] (kbd "RET")))

;; tabbar binds
(global-set-key [(control tab)] 'tabbar-forward-tab)
(global-set-key (kbd "<C-iso-lefttab>") 'tabbar-backward-tab)
(global-set-key (kbd "M-s-<up>") 'tabbar-forward-group)
(global-set-key (kbd "M-s-<down>") 'tabbar-backward-group)
(global-set-key (kbd "C-w") 'kill-this-buffer)  ;; additional tab-like close

;; browse kill ring
(global-set-key (kbd "C-c y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c C-y") 'helm-show-kill-ring)

;; text operation
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; web mode custom binds
(define-key web-mode-map (kbd "C-t") 'web-mode-tag-match)  ; grab and toggle tags
(define-key web-mode-map (kbd "C-<down>") 'web-mode-element-next)
(define-key web-mode-map (kbd "C-<up>") 'web-mode-element-previous)

;; emmet-mode
(define-key web-mode-map (kbd "M-;") 'emmet-expand-line)
(global-set-key (kbd "M-;") 'emmet-expand-line)
(define-key emmet-mode-keymap (kbd "C-j") 'grim/newline-and-indent-inside-of-bracket)

;; indent/unindent
(global-set-key (kbd "M->") 'custom-indent-region)
(global-set-key (kbd "M-<") 'custom-unindent-region)

;; calfw
(define-key cfw:calendar-mode-map (kbd "<SPC>") 'nil)
(define-key cfw:calendar-mode-map (kbd "o") 'cfw:show-details-command)

;; org-mode
(define-key org-mode-map (kbd "C-l") 'ace-link-org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)

(define-key org-mode-map [(control tab)] 'tabbar-forward-tab)
(define-key org-mode-map (kbd "<C-iso-lefttab>") 'tabbar-backward-tab)
(define-key org-mode-map (kbd "C-c C-r") 'org-refile)
(evil-define-key 'normal org-mode-map (kbd "R") 'org-refile)

;; org-agenda
(add-hook 'org-agenda-mode-hook
          (lambda()
            (define-key org-agenda-mode-map (kbd "j") 'org-agenda-next-line)
            (define-key org-agenda-mode-map (kbd "k") 'org-agenda-previous-line)
            (define-key org-agenda-mode-map (kbd "C-k") 'scroll-down-command)
            (define-key org-agenda-mode-map (kbd "C-j") 'scroll-up-command)
            (define-key org-agenda-mode-map (kbd "h") 'hydra-org-agenda/body)
            (define-key org-agenda-mode-map (kbd "M") 'org-agenda-month-view)))

;; move text up and down
(global-set-key (kbd "M-<up>") 'drag-stuff-up)
(global-set-key (kbd "M-<down>") 'drag-stuff-down)
(global-set-key (kbd "M-<left>") 'drag-stuff-left)
(global-set-key (kbd "M-<right>") 'drag-stuff-right)
(define-key evil-normal-state-map (kbd "M-k") 'drag-stuff-up)
(define-key evil-normal-state-map (kbd "M-j") 'drag-stuff-down)

;; yas expand
;; company ring map
(define-prefix-command 'completion-ring-map)
(global-unset-key (kbd "M-<SPC>"))
(global-set-key (kbd "M-<SPC>") 'completion-ring-map)
;; company mode tab fix
(global-set-key [tab] 'tab-indent-or-complete)
(global-set-key (kbd "TAB") 'tab-indent-or-complete)
(global-set-key (kbd "C-<SPC>") 'company-dabbrev-code)
(global-set-key (kbd "M-<SPC> <SPC>") 'company-complete-common)
(global-set-key (kbd "M-<SPC> j") 'company-complete)
(global-set-key (kbd "M-<SPC> y") 'hydra-yasnippet/body)
(global-set-key (kbd "M-<SPC> s") 'company-yasnippet)
(global-set-key (kbd "M-<SPC> g") 'company-gtags)
(global-set-key (kbd "M-<SPC> f") 'company-files)
(global-set-key (kbd "M-<SPC> a") 'aya-create)
(global-set-key (kbd "M-<SPC> e") 'aya-expand)
(global-set-key (kbd "M-<SPC> o") 'aya-open-line)
;;evil plus company synergy
(define-key evil-insert-state-map (kbd "C-<SPC>") 'company-dabbrev-code)
(define-key evil-insert-state-map (kbd "M-<SPC> <SPC>") 'company-complete-common)
(define-key evil-insert-state-map (kbd "M-<SPC> j") 'company-complete)
(define-key evil-insert-state-map (kbd "M-<SPC> y") 'hydra-yasnippet/body)
(define-key evil-insert-state-map (kbd "M-<SPC> s") 'company-yasnippet)
(define-key evil-insert-state-map (kbd "M-<SPC> g") 'company-gtags)
(define-key evil-insert-state-map (kbd "M-<SPC> f") 'company-files)
(define-key evil-insert-state-map (kbd "M-<SPC> a") 'aya-create)
(define-key evil-insert-state-map (kbd "M-<SPC> e") 'aya-expand)
(define-key evil-insert-state-map (kbd "M-<SPC> o") 'aya-open-line)

(define-key company-active-map [tab] 'expand-snippet-or-complete-selection)
(define-key company-active-map (kbd "TAB") 'expand-snippet-or-complete-selection)

(define-key yas-minor-mode-map [tab] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(define-key yas-keymap [tab] 'yas-next-field-or-maybe-expand)
(define-key yas-keymap (kbd "TAB") 'yas-next-field-or-maybe-expand)
(define-key yas-keymap [(control tab)] 'nil)
(define-key yas-keymap (kbd "C-g") 'abort-company-or-yas)
;; evilify company mode
(define-key company-active-map (kbd "M-j") 'company-select-next)
(define-key company-active-map (kbd "M-k") 'company-select-previous)
(define-key company-active-map [escape] 'keyboard-quit-and-remove-evil-mc)

;; evilify dired
(define-key dired-mode-map (kbd "TAB") 'other-window)
(define-key dired-mode-map [tab] 'other-window)

(define-key dired-mode-map (kbd "RET") 'dired-find-file)
(define-key dired-mode-map (kbd "<backspace>") 'dired-up-directory)

(define-key dired-mode-map (kbd "/") 'dired-narrow-fuzzy)
(define-key dired-mode-map (kbd "M-f") 'dired-narrow-fuzzy)
(define-key dired-mode-map (kbd "M-e") 'avy-goto-char-timer)
(define-key dired-mode-map (kbd "M-j") 'avy-goto-word-or-subword-1)

(define-key dired-mode-map (kbd "j") 'diredp-next-line)
(define-key dired-mode-map (kbd "C-j") 'scroll-up-command)
(define-key dired-mode-map (kbd "k") 'diredp-previous-line)
(define-key dired-mode-map (kbd "C-k") 'scroll-down-command)
(define-key dired-mode-map (kbd "h") 'diredp-up-directory-reuse-dir-buffer)
(define-key dired-mode-map (kbd "l") 'diredp-find-file-reuse-dir-buffer)
(define-key dired-mode-map (kbd "C-x M-o") 'dired-omit-switch)
(define-key dired-mode-map (kbd "C-o") 'dired-omit-switch)

(define-key dired-mode-map (kbd "I") 'dired-kill-subdir)

(define-key dired-mode-map (kbd "C-<right>") 'delete-other-windows-and-split-right)
(define-key global-map (kbd "C-<right>") 'delete-other-windows-and-split-right)

;; dired jump
(global-set-key (kbd "C-;") 'dired-jump)
;; dired sort
(define-key dired-mode-map (kbd "s") 'xah-dired-sort)

;; better new line from inside of a bracket
(global-set-key (kbd "C-j") 'grim/newline-and-indent-inside-of-bracket)

;; eyebrowse
(evil-define-key 'normal eyebrowse-mode-map (kbd "gt") 'eyebrowse-next-window-config)
(evil-define-key 'normal eyebrowse-mode-map (kbd "gT") 'eyebrowse-prev-window-config)
(evil-define-key 'normal eyebrowse-mode-map (kbd "gc") 'eyebrowse-close-window-config)
(evil-define-key 'normal eyebrowse-mode-map (kbd "g.") 'eyebrowse-switch-to-window-config)

;; evil mc
(evil-define-key 'normal evil-mc-key-map (kbd "M-p") 'always-paste-from-j)
(evil-define-key 'visual evil-mc-key-map (kbd "M-p") 'always-paste-from-j)
(evil-define-key 'insert evil-mc-key-map (kbd "M-p") 'always-paste-from-j)
(evil-define-key 'normal evil-mc-key-map (kbd "C-p") 'evil-paste-pop-next)
(evil-define-key 'visual evil-mc-key-map (kbd "C-p") 'evil-paste-pop-next)
(evil-define-key 'insert evil-mc-key-map (kbd "C-p") 'evil-paste-pop-next)
(global-set-key (kbd "C-S-<mouse-1>") 'evil-mc-toggle-cursor-on-click)
(evil-define-key 'normal helm-multi-swoop-edit-map (kbd "C-n") 'evil-mc-make-and-goto-next-match)

;; evil numbers
(define-key evil-normal-state-map (kbd "+") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "-") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "<kp-add>") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "<kp-subtract>") 'evil-numbers/dec-at-pt)

;; evil-mode
(global-set-key (kbd "<f2>") 'evil-mode)
(evil-define-key 'normal quickrun--mode-map (kbd "q") 'quit-window)
;; scroll through visual lines
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
;; evil-jump-forward rebind
(define-key evil-normal-state-map (kbd "C-S-o") 'evil-jump-forward)
(define-key evil-visual-state-map (kbd "C-S-o") 'evil-jump-forward)
;; scroll up/down with C-k, C-j
(define-key evil-normal-state-map (kbd "C-k") (lambda ()
                                                (interactive)
                                                (evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "C-j") (lambda ()
                                                (interactive)
                                                (evil-scroll-down nil)))
(define-key evil-motion-state-map (kbd "C-k") (lambda ()
                                                (interactive)
                                                (evil-scroll-up nil)))
(define-key evil-motion-state-map (kbd "C-j") (lambda ()
                                                (interactive)
                                                (evil-scroll-down nil)))
(define-key evil-normal-state-map (kbd "SPC h") help-map)

;; esc anything
(define-key evil-normal-state-map [escape] 'keyboard-quit-and-remove-evil-mc)
(define-key evil-visual-state-map [escape] 'keyboard-quit-and-remove-evil-mc)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(global-set-key [escape] 'evil-exit-emacs-state)

;; evil args
(define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
(define-key evil-outer-text-objects-map "a" 'evil-outer-arg)
(define-key evil-normal-state-map "L" 'evil-forward-arg)
(define-key evil-normal-state-map "H" 'evil-backward-arg)
(define-key evil-motion-state-map "L" 'evil-forward-arg)
(define-key evil-motion-state-map "H" 'evil-backward-arg)
(define-key evil-normal-state-map "K" 'evil-jump-out-args)

;; evil leader bindings
(evil-leader/set-key
  "`" 'show-file-name
  "f" 'helm-swoop-without-pre-input
  "F" 'helm-find-files
  "M-f" 'helm-multi-swoop-all
  "s" (lambda() (interactive) (save-some-buffers t))
  "t" 'hydra-rare-launcher/body
  "pp" 'helm-projectile-switch-project
  "pf" 'helm-projectile-find-file
  "pF" 'helm-projectile-find-file-in-known-projects
  "pi" 'projectile-invalidate-cache
  "pk" 'projectile-kill-buffers
  "pa" 'helm-projectile-ag
  "D" 'dired
  "d" 'hydra-window-operations/body
  "q" 'winner-undo
  "Q" 'winner-redo
  "M-q" 'clean-buffer-list
  "e" 'avy-goto-char-timer
  "E" 'evil-avy-goto-char-2
  "j" 'evil-avy-goto-word-or-subword-1
  "J" 'evil-avy-goto-word-0
  "a" 'anzu-query-replace
  "A" 'anzu-query-replace-at-cursor
  ";" 'evilnc-comment-or-uncomment-lines
  "kk" 'evilnc-comment-or-uncomment-lines
  "kl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "kc" 'evilnc-copy-and-comment-lines
  "kp" 'evilnc-comment-or-uncomment-paragraphs
  "kr" 'comment-or-uncomment-region
  "kv" 'evilnc-toggle-invert-comment-line-by-line
  "x" 'helm-M-x
  "w" 'helm-mini
  "W" 'whitespace-mode
  "b" 'helm-filtered-bookmarks
  "m" 'helm-all-mark-rings
  "i" 'cleanup-buffer
  "I" 'hydra-indent/body
  "r" 'helm-semantic-or-imenu
  "R" 'helm-imenu-anywhere
  "lo" 'dumb-jump-go-other-window
  "lj" 'dumb-jump-go
  "lx" 'dumb-jump-go-prefer-external
  "lz" 'dumb-jump-go-prefer-external-other-window
  "lq" 'dumb-jump-quick-look
  "y" 'helm-show-kill-ring
  "Y" 'helm-register
  "hg" 'helm-do-grep-ag
  "hG" 'helm-google-suggest
  "ha" 'helm-do-ag
  "hl" 'helm-locate
  "hw" 'helm-do-ag-buffers
  "hd" 'helm-dash
  "hs" 'helm-system-packages
  "gs" 'magit-status
  "gi" 'magit-init
  "gl" 'magit-log-popup
  "gr" 'magit-list-repositories
  "gm" 'git-messenger:popup-message
  "gM" 'git-messenger:popup-show-verbose
  "gc" 'hydra-smerge/body
  "oc" 'org-capture
  "ol" 'org-store-link
  "oA" 'org-agenda-list
  "oa" 'org-agenda-list-and-delete-other-windows
  "ob" 'org-iswitchb
  "ok" 'cfw:open-org-calendar
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

(evil-leader/set-key-for-mode 'web-mode
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

(evil-leader/set-key-for-mode 'org-mode
  "cf" 'helm-org-rifle)

(evil-leader/set-key-for-mode 'org-agenda-mode
  "oa" 'hydra-org-agenda/body)

(evil-leader/set-key-for-mode 'js2-mode
  "ci" 'js-doc-insert-function-doc-snippet
  "cc" 'tern-get-type
  "cr" 'tern-rename-variable
  "c." 'tern-find-definition
  "c," 'tern-pop-find-definition
  "cd" 'tern-get-docs)

(evil-leader/set-key-for-mode 'typescript-mode
  "c" 'tide-documentation-at-point)

(evil-leader/set-key-for-mode 'message-mode
  "w" 'helm-buffer-list
  "d" 'ace-window)

(evil-leader/set-key-for-mode 'pomidor-mode
  "<SPC>" 'pomidor-break)

;; evil god state
(evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
(evil-define-key 'god global-map [escape] 'evil-god-state-bail)

(provide 'key-bindings-setup)
;;; key-bindings-setup.el ends here
