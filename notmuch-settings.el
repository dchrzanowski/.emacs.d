;;; package -- Summary
;;; Commentary:
;; -------------------------------------------------------------------------------------------------------------------------
;;; Code:
;; -------------------------------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------------------------------
;; key binds for notmuch and other settings
;; -------------------------------------------------------------------------------------------------------------------------

;; notmuch
(global-set-key [f10] 'notmuch)

;; welcome mode
(evil-define-key 'normal notmuch-hello-mode-map
  (kbd "q") 'notmuch-bury-or-kill-this-buffer
  (kbd "R") 'notmuch-refresh-all-buffers
  (kbd "m") 'notmuch-mua-new-mail
  (kbd "/") 'notmuch-search
  (kbd "h") 'widget-backward
  (kbd "l") 'widget-forward
  (kbd "f") 'notmuch-jump-search
  (kbd "RET") 'widget-button-press)

;; search mode
(evil-define-key 'normal notmuch-search-mode-map
  (kbd "q") 'notmuch-bury-or-kill-this-buffer
  (kbd "R") 'notmuch-refresh-all-buffers
  (kbd "m") 'notmuch-mua-new-mail
  (kbd "/") 'notmuch-search
  (kbd "=") 'notmuch-show-add-tag
  (kbd "-") 'notmuch-show-remove-tag
  (kbd "o") 'notmuch-search-toggle-order
  (kbd "r") 'notmuch-search-reply-to-thread-sender
  (kbd "l") 'notmuch-search-show-thread
  (kbd "h") 'notmuch-bury-or-kill-this-buffer
  (kbd "n") 'notmuch-search-next-thread
  (kbd "p") 'notmuch-search-previous-thread
  (kbd "f") 'notmuch-jump-search)

(evil-leader/set-key-for-mode 'notmuch-search-mode
  "z" 'notmuch-tree-from-search-current-query
  "t" 'notmuch-search-filter-by-tag)

;; tree view
(evil-define-key 'normal notmuch-tree-mode-map
  (kbd "q") 'notmuch-bury-or-kill-this-buffer
  (kbd "R") 'notmuch-refresh-all-buffers
  (kbd "m") 'notmuch-mua-new-mail
  (kbd "/") 'notmuch-search
  (kbd "=") 'notmuch-show-add-tag
  (kbd "-") 'notmuch-show-remove-tag
  (kbd "o") 'notmuch-search-toggle-order
  (kbd "t") 'notmuch-search-filter-by-tag
  (kbd "r") 'notmuch-search-reply-to-thread-sender
  (kbd "l") 'notmuch-tree-show-message
  (kbd "h") 'notmuch-bury-or-kill-this-buffer
  (kbd "n") 'notmuch-search-next-thread
  (kbd "p") 'notmuch-search-previous-thread
  (kbd "f") 'notmuch-jump-search)

(evil-leader/set-key-for-mode 'notmuch-tree-mode
  "z" 'notmuch-tree-from-search-current-query
  "t" 'notmuch-search-filter-by-tag)

;; show message mode
(evil-define-key 'normal notmuch-show-mode-map
  (kbd "q") 'notmuch-bury-or-kill-this-buffer
  (kbd "R") 'notmuch-refresh-all-buffers
  (kbd "m") 'notmuch-mua-new-mail
  (kbd "/") 'notmuch-search
  (kbd "=") 'notmuch-show-add-tag
  (kbd "-") 'notmuch-show-remove-tag
  (kbd "r") 'notmuch-show-reply-sender
  (kbd "h") 'notmuch-bury-or-kill-this-buffer
  (kbd "f") 'notmuch-jump-search)

(provide 'notmuch-settings)
;;; notmuch-settings ends here
