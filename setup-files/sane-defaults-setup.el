;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; Turn off mouse interface early in startup to avoid momentary display
;; --------------------------------------------------------------------
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; --------------------------------------------------------------------
;; emacs system settigs
;; --------------------------------------------------------------------
;; UTF-8 please
(set-charset-priority 'unicode)
(setq locale-coding-system   'utf-8)   ; pretty
(set-terminal-coding-system  'utf-8)   ; pretty
(set-keyboard-coding-system  'utf-8)   ; pretty
(set-selection-coding-system 'utf-8)   ; please
(prefer-coding-system        'utf-8)   ; with sugar on top
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; backup config
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))   ; create a special folder for backup files
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5)    ; and how many of the old


;; holidays disabled
(setq holiday-bahai-holidays nil
      holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-oriental-holidays nil
      holiday-general-holidays nil
      holiday-christian-holidays nil
      holiday-local-holidays nil
      holiday-other-holidays nil
      holiday-solar-holidays nil)

;; No splash screen
(setq inhibit-startup-message t)
;; delete when region when starting to type inside
(delete-selection-mode t)
;; mark follows the point
(transient-mark-mode t)
;; share kill ring with the system's clipboard
(setq select-enable-clipboard t
      x-select-enable-clipboard-manager nil)
;; switch off bell
(setq ring-bell-function 'ignore)
;; save history (minibuffer)
(savehist-mode 1)
;; scroll through visual lines
(global-visual-line-mode)
;; remove slow on scroll
(setq-default auto-window-vscroll nil)
;; show column numbers
(column-number-mode t)
;; winner mode (undo/redo frames)
(when (fboundp 'winner-mode)
  (winner-mode 1))
;; save opened buffers (and window config)
(desktop-save-mode t)
;; auto indent
(electric-indent-mode t)
;; show matching brackets
(show-paren-mode t)
;; highlight current line
(global-hl-line-mode)
;; auto refresh file when changed on disk
(global-auto-revert-mode t)
;; keep auto revert quiet
(setq-default auto-revert-verbose nil)
;; auto-focus help windows, easier to Q them
(setq-default help-window-select t)
;; (global-subword-mode)  ;; iterate through camelcase
(setq disabled-command-function nil)

;; --------------------------------------------------------------------
;; indents config
;; --------------------------------------------------------------------
(setq-default indent-tabs-mode nil)  ;; do not insert tabs
(setq-default sgml-basic-offset 4)  ;; indent for html
(setq-default tab-width 4)   ; standard tab width
(setq-default c-basic-offset 4)  ;; standard width for c/C++
(c-set-offset 'substatement-open 0) ;; fix c/c++ indent

;; --------------------------------------------------------------------
;; misc
;; --------------------------------------------------------------------
;; remove annoying minibuffer prompts
;; (setq minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
(cursor-intangible-mode 1)
;; ask y/s  instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; --------------------------------------------------------------------
;; semantic mode
;; --------------------------------------------------------------------
(semantic-mode 1)

;; --------------------------------------------------------------------
;; notifications
;; --------------------------------------------------------------------
(setq alert-default-style 'libnotify)

(provide 'sane-defaults-setup)
;;; sane-defaults-setup.el ends here
