;;; Commentary:
;; -------------------------------------------------------------------------------------------------------------------------
;;; Code:
;; -------------------------------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------------------------------
;; key binds / shortcuts
;; -------------------------------------------------------------------------------------------------------------------------

;; neotree
(global-set-key [f8] 'neotree-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "l") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "L") 'neotree-enter-ace-window)
(evil-define-key 'normal neotree-mode-map (kbd "h") 'neotree-select-up-node)
(evil-define-key 'normal neotree-mode-map (kbd "o") 'neotree-hidden-file-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "r") 'neotree-change-root)
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

;; packages
(global-set-key (kbd "M-[") 'package-install)
(global-set-key (kbd "M-]") 'list-packages)

;; open init with F6
(global-set-key (kbd "<f6>") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))

;;anzu binds
(global-set-key (kbd "C-f") 'anzu-query-replace)
(global-set-key (kbd "C-M-r") 'anzu-query-replace-at-cursor-thing)
(global-set-key (kbd "C-M-s-r") 'anzu-query-replace-at-cursor)

(global-set-key (kbd "C-M->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;;rainbow mode
(global-set-key (kbd "C-c r") 'rainbow-mode)

;; ace window
(global-set-key (kbd "M-d") 'ace-window)
(define-key dired-mode-map (kbd "M-d") 'ace-window)
(define-key sr-mode-map (kbd "M-d") 'ace-window)

;; changed recenter
(define-key global-map (kbd "C-l") 'recenter)

;; helm bindings
(global-set-key (kbd "M-x") 'helm-M-x)
(with-eval-after-load 'magit-mode
  (define-key magit-mode-map (kbd "M-x") 'helm-M-x))

(global-set-key (kbd "M-w") 'helm-buffers-list)

;; helm-swoop binds
(global-set-key (kbd "M-f") 'helm-swoop-without-pre-input)
(global-set-key (kbd "C-M-f") 'helm-multi-swoop)
(global-set-key (kbd "C-M-S-f") 'helm-multi-swoop-all)

;; avy
(global-set-key (kbd "M-e") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "M-j") 'avy-goto-char)

;; helm bookmark
(global-set-key (kbd "C-c b") 'helm-bookmarks)
(global-set-key (kbd "C-c C-b") 'helm-bookmarks)

;; vim like helm
(define-key helm-map (kbd "M-j") 'helm-next-line)
(define-key helm-map (kbd "M-k") 'helm-previous-line)
(define-key helm-map (kbd "M-h") 'helm-next-source)
(define-key helm-map (kbd "C-S-h") 'describe-key)
(define-key helm-map (kbd "M-l") (kbd "RET"))
(define-key helm-map [escape] 'helm-keyboard-quit)
(dolist (keymap (list helm-find-files-map helm-read-file-map helm-generic-files-map))
  (define-key keymap (kbd "M-l") 'helm-execute-persistent-action)
  (define-key keymap (kbd "M-h") 'helm-find-files-up-one-level)
  (define-key keymap (kbd "M-J") 'helm-ff-run-open-file-with-default-tool)
  (define-key keymap (kbd "C-S-h") 'describe-key))

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

;; expand region
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-RET") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)

;; web mode custom binds
(define-key web-mode-map (kbd "C-t") 'web-mode-tag-match)  ; grab and toggle tags
(define-key web-mode-map (kbd "C-<down>") 'web-mode-element-next)
(define-key web-mode-map (kbd "C-<up>") 'web-mode-element-previous)

;; indent and unindent
(global-set-key (kbd "M->") 'my-indent-region)
(global-set-key (kbd "M-<") 'my-unindent-region)

;; org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key org-mode-map [(control tab)] 'tabbar-forward-tab)
(define-key org-mode-map (kbd "<C-iso-lefttab>") 'tabbar-backward-tab)

;; Comment/uncomment block
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; move text up and down
(global-set-key (kbd "M-<up>") 'drag-stuff-up)
(global-set-key (kbd "M-<down>") 'drag-stuff-down)
(global-set-key (kbd "M-<left>") 'drag-stuff-left)
(global-set-key (kbd "M-<right>") 'drag-stuff-right)

;; eclim
(define-key eclim-mode-map (kbd "C-c C-e <f5>") 'eclim-run-class)
(define-key eclim-mode-map (kbd "C-c C-e /") 'eclim-java-show-documentation-for-current-element)

;; yas expand
;; company ring map
(define-prefix-command 'ring-map)
(global-unset-key (kbd "M-<SPC>"))
(global-set-key (kbd "M-<SPC>") 'ring-map)
;; company mode tab fix
(global-set-key [tab] 'tab-indent-or-complete)
(global-set-key (kbd "TAB") 'tab-indent-or-complete)
(global-set-key (kbd "C-<SPC>") 'company-complete-common)
(global-set-key (kbd "M-<SPC> <SPC>") 'company-yasnippet)
(global-set-key (kbd "M-<SPC> g") 'company-gtags)
(global-set-key (kbd "M-<SPC> f") 'company-files)

(define-key company-active-map [tab] 'expand-snippet-or-complete-selection)
(define-key company-active-map (kbd "TAB") 'expand-snippet-or-complete-selection)

(define-key yas-minor-mode-map [tab] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(define-key yas-keymap [tab] 'yas-next-field-or-maybe-expand)
(define-key yas-keymap (kbd "TAB") 'yas-next-field-or-maybe-expand)
(define-key yas-keymap [(control tab)] 'nil)
(define-key yas-keymap (kbd "C-g") 'abort-company-or-yas)
;; company mode
(define-key company-active-map (kbd "M-j") 'company-select-next)
(define-key company-active-map (kbd "M-k") 'company-select-previous)
(define-key company-active-map [escape] 'company-abort)

;; dired and sunrise fixes
(define-key sr-mode-map (kbd "<f2>") 'evil-mode)
(define-key dired-mode-map (kbd "TAB") 'other-window)
(define-key dired-mode-map [tab] 'other-window)
(define-key sr-mode-map (kbd "TAB") 'sr-change-window)
(define-key sr-mode-map [tab] 'sr-change-window)

(define-key dired-mode-map (kbd "RET") 'dired-find-file)
(define-key sr-mode-map (kbd "(") 'sr-toggle-attributes)

(define-key dired-mode-map (kbd "/") 'dired-narrow-fuzzy)
(define-key sr-mode-map (kbd "/") 'dired-narrow-fuzzy)
(define-key dired-mode-map (kbd "M-f") 'dired-narrow-fuzzy)
(define-key sr-mode-map (kbd "M-f") 'dired-narrow-fuzzy)
(define-key dired-mode-map (kbd "M-e") 'avy-goto-word-or-subword-1)
(define-key sr-mode-map (kbd "M-e") 'avy-goto-word-or-subword-1)
(define-key dired-mode-map (kbd "M-j") 'avy-goto-char)
(define-key sr-mode-map (kbd "M-j") 'avy-goto-char)

(define-key dired-mode-map (kbd "j") 'diredp-next-line)
(define-key sr-mode-map (kbd "j") 'diredp-next-line)
(define-key dired-mode-map (kbd "k") 'diredp-previous-line)
(define-key sr-mode-map (kbd "k") 'diredp-previous-line)
(define-key dired-mode-map (kbd "h") 'diredp-up-directory-reuse-dir-buffer)
(define-key sr-mode-map (kbd "h") 'sr-dired-prev-subdir)
(define-key dired-mode-map (kbd "l") 'diredp-find-file-reuse-dir-buffer)
(define-key sr-mode-map (kbd "l") 'sr-advertised-find-file)
(define-key dired-mode-map (kbd "C-x M-o") 'dired-omit-switch)

;;palette
(global-set-key (kbd "C-<f9>") 'palette-launch-from-kill-ring)
(global-set-key (kbd "C-S-<f9>") 'palette-paste-in-current-color)

;; better new line form inside of a bracket
(global-set-key (kbd "C-j") 'my-fancy-newline)

;; evil-mode
(global-set-key (kbd "<f2>") 'evil-mode)
(evil-define-key 'normal quickrun--mode-map (kbd "q") 'quit-window)

(define-key evil-normal-state-map (kbd "M-p") 'always-paste-zero)
(define-key evil-visual-state-map (kbd "M-p") 'always-paste-zero)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)  ;; scroll through visual lines
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

(define-key evil-insert-state-map (kbd "C-<SPC>") 'company-complete-common)
(define-key evil-insert-state-map (kbd "M-<SPC> <SPC>") 'company-yasnippet)
(define-key evil-insert-state-map (kbd "M-<SPC> g") 'company-gtags)
(define-key evil-insert-state-map (kbd "M-<SPC> f") 'company-files)

;; scroll up/down with C-k, C-j
(define-key evil-normal-state-map (kbd "C-k") (lambda ()
                                                (interactive)
                                                (evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "C-j") (lambda ()
                                                (interactive)
                                                (evil-scroll-down nil)))
(define-key evil-normal-state-map (kbd "SPC h") help-map)

;; esc anything
(define-key evil-normal-state-map [escape] 'my-keyboard-quit)
(define-key evil-visual-state-map [escape] 'my-keyboard-quit)
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
  "f" 'helm-swoop-without-pre-input
  "s" (lambda() (interactive) (save-some-buffers t))
  "F" 'helm-find-files
  "pp" 'helm-projectile-switch-project
  "pf" 'helm-projectile-find-file
  "pF" 'helm-projectile-find-file-in-known-projects
  "pi" 'projectile-invalidate-cache
  "pk" 'projectile-kill-buffers
  "pa" 'helm-projectile-ag
  "D" 'dired
  "d" 'ace-window
  "e" 'evil-avy-goto-word-or-subword-1
  "E" 'evil-avy-goto-word-0
  "j" 'evil-avy-goto-char
  "J" 'evil-avy-goto-char-2
  "a" 'anzu-query-replace
  "A" 'anzu-query-replace-at-cursor
  ";" 'comment-or-uncomment-region
  "k" 'comment-line
  "x" 'helm-M-x
  "w" 'helm-buffers-list
  "b" 'helm-bookmarks
  "n" 'cleanup-buffer
  "r" 'helm-semantic-or-imenu
  "hg" 'helm-do-grep-ag
  "ha" 'helm-do-ag
  "hl" 'helm-locate
  "hw" 'helm-do-ag-buffers
  "gs" 'magit-status
  "gi" 'magit-init
  "gl" 'magit-log-popup
  "tn" 'ahs-forward
  "tp" 'ahs-backward
  "te" 'ahs-edit-mode)

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

;; evil god state
(evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
(evil-define-key 'god global-map [escape] 'evil-god-state-bail)

(provide 'my-bindings)
;;; my-bindings.el ends here
