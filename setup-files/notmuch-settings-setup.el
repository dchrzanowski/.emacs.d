;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; settings and key bindings for notmuch
;; --------------------------------------------------------------------

(use-package notmuch
  :defer t
  :bind ("f10" . 'notmuch)
  :config
  (require 'notmuch-settings-setup)

  (setq notmuch-search-oldest-first nil
        mail-user-agent 'message-user-agent
        smtpmail-stream-type 'ssl
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 465
        message-send-mail-function 'message-smtpmail-send-it
        smtpmail-debug-info t
        message-default-mail-headers "Cc: \nBcc: \n"
        message-auto-save-directory "~/Mail/pjdamian/draft"
        message-kill-buffer-on-exit t
        message-directory "~/Mail/pjdamian/draft"
        notmuch-fcc-dirs "pjdamian/sent"
        user-mail-address "pjdamian.chrzanowski@gmail.com")

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
    (kbd "f") 'notmuch-jump-search))

(provide 'notmuch-settings-setup)
;;; notmuch-settings-setup ends here
