;;; Commentary:
;; -------------------------------------------------------------------------------------------------------------------------
;;; Code:
;; -------------------------------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------------------------------
;; key binds / shortcuts
;; -------------------------------------------------------------------------------------------------------------------------
(global-set-key [f8] 'neotree-toggle)
(global-set-key [f9] 'sunrise)
(global-set-key [f5]
                (lambda ()
                  (interactive)
                  (save-window-excursion
                    (async-shell-command (concat "urxvt -e run_any.py " default-directory " " (buffer-name))))))
(global-set-key (kbd "C-w") 'kill-this-buffer)

;; windmove
(global-set-key (kbd "C-x <left>") 'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <up>")  'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)

;; define a prefix for window split
;; (define-prefix-command 'ring-map)
;; (global-set-key (kbd "C-s") 'ring-map)

;; split windows
;; (global-set-key (kbd "C-s <left>") 'split-window-vertically)
;; (global-set-key (kbd "C-s <right>") 'split-window-right)
;; (global-set-key (kbd "C-s <up>")  'split-window-horizontally)
;; (global-set-key (kbd "C-s <down>") 'split-window-below)

;;anzu binds
(global-set-key (kbd "C-f") 'anzu-query-replace)
(global-set-key (kbd "C-M-r") 'anzu-query-replace-at-cursor-thing)
(global-set-key (kbd "C-M-s-r") 'anzu-query-replace-at-cursor)

;; multicursor
;;(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
;;  java-mode fix
(defun on-java-loaded ()
  "Fixes java mode not responding to the C d key."
  (define-key java-mode-map (kbd "C->") 'mc/mark-next-like-this)
  (define-key java-mode-map (kbd "C-<") 'mc/mark-previous-like-this))
(add-hook 'java-mode-hook 'on-java-loaded)

(global-set-key (kbd "C-M->") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;;rainbow mode
(global-set-key (kbd "C-c r") 'rainbow-mode)

;; packages
(global-set-key (kbd "M-[") 'package-install)
(global-set-key (kbd "M-]") 'list-packages)

;; open setting with F6
(global-set-key (kbd "<f6>") (lambda() (interactive)(find-file "~/.emacs.d/init.el")))

;; ace jump keybinds
(define-key global-map (kbd "M-e") 'ace-jump-mode)
(define-key global-map (kbd "M-a") 'ace-jump-line-mode)
;; (key-chord-define-global (kbd "jj") 'ace-jump-mode)

;; ace window
(global-set-key (kbd "M-q") 'ace-window)
(define-key dired-mode-map (kbd "M-q") 'ace-window)
(define-key sr-mode-map (kbd "M-q") 'ace-window)

;; changed recenter
(define-key global-map (kbd "C-l") 'recenter)

;; helm bindings
(global-set-key (kbd "M-w") 'helm-M-x)
(with-eval-after-load 'magit-mode
  (define-key magit-mode-map (kbd "M-w") 'helm-M-x))

(global-set-key (kbd "M-x") 'helm-buffers-list)

;; helm-swoop binds
(global-set-key (kbd "M-f") 'helm-swoop-without-pre-input)
(global-set-key (kbd "C-M-f") 'helm-multi-swoop)
(global-set-key (kbd "C-M-S-f") 'helm-multi-swoop-all)

;; helm bookmark
(global-set-key (kbd "C-c b") 'helm-bookmarks)
(global-set-key (kbd "C-c C-b") 'helm-bookmarks)

;; vim like for helm
(define-key helm-map (kbd "M-j") 'helm-next-line)
(define-key helm-map (kbd "M-k") 'helm-previous-line)
(define-key helm-map (kbd "M-h") 'helm-next-source)
(define-key helm-map (kbd "C-S-h") 'describe-key)
(define-key helm-map (kbd "M-l") (kbd "RET"))
(define-key helm-map [escape] 'helm-keyboard-quit)
(dolist (keymap (list helm-find-files-map helm-read-file-map))
  (define-key keymap (kbd "M-l") 'helm-execute-persistent-action)
  (define-key keymap (kbd "M-h") 'helm-find-files-up-one-level)
  (define-key keymap (kbd "C-S-h") 'describe-key))

;; tabbar binds
(global-set-key [(control tab)] 'tabbar-forward-tab)
(global-set-key (kbd "<C-iso-lefttab>") 'tabbar-backward-tab)
(global-set-key (kbd "M-s-<up>") 'tabbar-forward-group)
(global-set-key (kbd "M-s-<down>") 'tabbar-backward-group)

;; semantic search with helm
(global-set-key (kbd "C-r") 'helm-semantic-or-imenu)

;; browse kill ring
(global-set-key (kbd "C-c y") 'helm-show-kill-ring)
(global-set-key (kbd "C-c C-y") 'helm-show-kill-ring)

;; beautify
(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

;; text operation
(global-set-key (kbd "C-a") 'mark-whole-buffer)

;; expand region
(global-set-key (kbd "C-=") 'er/expand-region)
(global-set-key (kbd "M-RET") 'er/expand-region)
(global-set-key (kbd "C--") 'er/contract-region)
;; (key-chord-define-global "er" 'er/expand-region)
;; (key-chord-define-global "rt" 'er/contractb-region)

;; iy-go-to-char
(global-set-key (kbd "C-.") 'iy-go-to-char)
(global-set-key (kbd "C-,") 'iy-go-to-char-backward)

;; web mode custom binds
(define-key web-mode-map (kbd "C-t") 'web-mode-tag-match)  ; grab and toggle tags
(define-key web-mode-map (kbd "C-<down>") 'web-mode-element-next)
(define-key web-mode-map (kbd "C-<up>") 'web-mode-element-previous)

;; auto highlight
(global-set-key (kbd "C-p") 'ahs-backward)
(global-set-key (kbd "C-n") 'ahs-forward)
(global-set-key (kbd "C-b") 'ahs-edit-mode)

;; undo tree
(global-unset-key (kbd "C-/"))
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-y") 'undo-tree-redo)

;; indent and unindent
(global-set-key (kbd "M->") 'my-indent-region)
(global-set-key (kbd "M-<") 'my-unindent-region)

;; org-mode
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(define-key org-mode-map (kbd "M-e") 'ace-jump-mode)
(define-key org-mode-map (kbd "M-a") 'ace-jump-line-mode)
(define-key org-mode-map [(control tab)] 'tabbar-forward-tab)
(define-key org-mode-map (kbd "<C-iso-lefttab>") 'tabbar-backward-tab)

;; unset keys
(global-unset-key (kbd "C-/"))

;; Comment/uncomment block
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)
(define-key global-map (kbd "C-/") 'comment-or-uncomment-region)

;; move text up and down
(global-set-key (kbd "M-<up>") 'drag-stuff-up)
(global-set-key (kbd "M-<down>") 'drag-stuff-down)
(global-set-key (kbd "M-<left>") 'drag-stuff-left)
(global-set-key (kbd "M-<right>") 'drag-stuff-right)

;; eclim
(define-key eclim-mode-map (kbd "C-c C-e <f5>") 'eclim-run-class)
(define-key eclim-mode-map (kbd "C-c C-e /") 'eclim-java-show-documentation-for-current-element)

;; cua
(global-set-key (kbd "C-'") 'cua-set-mark)

;; yas expand
(global-set-key (kbd "M-<tab>") 'yas-expand)

;; company mode tab fix
(global-set-key [tab] 'tab-indent-or-complete)
(global-set-key (kbd "TAB") 'tab-indent-or-complete)
(global-set-key (kbd "C-<SPC>") 'company-complete-common)

(define-key company-active-map [tab] 'expand-snippet-or-complete-selection)
(define-key company-active-map (kbd "TAB") 'expand-snippet-or-complete-selection)
(define-key company-active-map (kbd "M-j") 'company-select-next)
(define-key company-active-map (kbd "M-k") 'company-select-previous)

(define-key yas-minor-mode-map [tab] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(define-key yas-keymap [tab] 'yas-next-field-or-maybe-expand)
(define-key yas-keymap (kbd "TAB") 'yas-next-field-or-maybe-expand)
(define-key yas-keymap [(control tab)] 'nil)
(define-key yas-keymap (kbd "C-g") 'abort-company-or-yas)

;; dired and sunrise fixes
(define-key dired-mode-map (kbd "TAB") 'other-window)
(define-key dired-mode-map [tab] 'other-window)
(define-key sr-mode-map (kbd "TAB") 'sr-change-window)
(define-key sr-mode-map [tab] 'sr-change-window)

(define-key dired-mode-map (kbd "/") 'dired-narrow-fuzzy)
(define-key sr-mode-map (kbd "/") 'dired-narrow-fuzzy)

;;palette
(global-set-key (kbd "C-<f9>") 'palette-launch-from-kill-ring)
(global-set-key (kbd "C-S-<f9>") 'palette-paste-in-current-color)

;; my support functions
(global-set-key (kbd "C-c k") 'delete-line-from-any-position)
(global-set-key (kbd "C-j") 'my-fancy-newline)
(global-set-key (kbd "C-S-j") 'end-of-line-and-indented-new-line)
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; evil-mode
(global-set-key (kbd "<f2>") 'evil-mode)
(define-key evil-normal-state-map (kbd "M-p") 'always-paste-zero)
(define-key evil-visual-state-map (kbd "M-p") 'always-paste-zero)
(define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)  ;; scroll through visual lines
(define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)
(define-key evil-insert-state-map (kbd "C-<SPC>") 'company-complete-common)

; scroll up/down with C-k, C-j
(define-key evil-normal-state-map (kbd "C-k") (lambda ()
                    (interactive)
                    (evil-scroll-up nil)))
(define-key evil-normal-state-map (kbd "C-j") (lambda ()
                        (interactive)
                        (evil-scroll-down nil)))
; SPC x -> C-x  , SPC h -> C-h   , SPC c -> C-c
;; (define-key evil-normal-state-map (kbd "SPC x") ctl-x-map)
(define-key evil-normal-state-map (kbd "SPC h") help-map)
;; (define-key evil-normal-state-map (kbd "SPC c")
;;    (lambda () (interactive)
;;      (setq unread-command-events
;;            (listify-key-sequence (kbd "C-c")))))

;; esc anything
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
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
 "f" 'helm-swoop
 "s" (lambda() (interactive) (save-some-buffers t))
 "F" 'helm-find-files
 "p" 'helm-projectile-switch-project
 "d" 'ace-window
 "e" 'ace-jump-mode
 "j" 'ace-jump-char-mode
 "a" 'anzu-query-replace
 "A" 'anzu-query-replace-at-cursor
 ";" 'comment-or-uncomment-region
 "k" 'comment-line
 "w" 'helm-M-x
 "b" 'helm-buffers-list
 "hg" 'helm-do-grep-ag
 "ha" 'helm-do-ag
 "gs" 'magit-status
 "gi" 'magit-init)

;; evil god state
(evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
(evil-define-key 'god global-map [escape] 'evil-god-state-bail)

(provide 'my-bindings)
;;; my-bindings.el ends here
