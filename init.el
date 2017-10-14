;;; package --- Summary

;; set a much higher GC collection threshold
;; (setq-default garbage-collection-messages t)
(setq gc-cons-threshold 100000000)

(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 10000000)))

;; -------------------------------------------------------------------------------------------------------------------------
;; initialize package repos and make sure that use-package is installed
;; -------------------------------------------------------------------------------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("SC" . "http://joseito.republika.pl/sunrise-commander/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)   ;; org mode specials
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; -------------------------------------------------------------------------------------------------------------------------
;; use-package config
;; -------------------------------------------------------------------------------------------------------------------------
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(require 'diminish)
(require 'bind-key)

;; -------------------------------------------------------------------------------------------------------------------------
;; emacs system settigs
;; -------------------------------------------------------------------------------------------------------------------------
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
      kept-old-versions 5    ; and how many of the old
      )

;; -------------------------------------------------------------------------------------------------------------------------
;; personal data
;; -------------------------------------------------------------------------------------------------------------------------
(setq user-full-name "Damian Chrzanowski")

;; -------------------------------------------------------------------------------------------------------------------------
;; Turn off mouse interface early in startup to avoid momentary display
;; -------------------------------------------------------------------------------------------------------------------------
(scroll-bar-mode -1)
(menu-bar-mode -1)
(tool-bar-mode -1)

;; -------------------------------------------------------------------------------------------------------------------------
;; Emacs defaults
;; -------------------------------------------------------------------------------------------------------------------------
(setq inhibit-startup-message t) ;; No splash screen
(delete-selection-mode t)  ;; delete when region when starting to type inside
(transient-mark-mode t)  ;; mark follows the point
(setq select-enable-clipboard t)  ;; share kill ring with the system's clipboard
(setq ring-bell-function 'ignore)  ;; switch off bell
(savehist-mode 1) ;; save history (minibuffer)
(global-visual-line-mode)   ;; scroll through visual lines
(setq-default auto-window-vscroll nil) ;; remove slow on scroll
(column-number-mode t) ;; show column numbers
(when (fboundp 'winner-mode)
  (winner-mode 1))  ;; winner mode (undo/redo frames)
(desktop-save-mode t)  ;; save opened buffers (and window config)
(electric-indent-mode t)  ;; auto indent
(show-paren-mode t)  ;; show matching brackets
(global-hl-line-mode)  ;; highlight current line
;; (global-subword-mode)  ;; iterate through camelcase
(global-auto-revert-mode t)  ;; auto refresh file when changed on disk
(setq-default auto-revert-verbose nil)  ;; keep auto revert quiet
(setq-default help-window-select t)  ;; auto-focus help windows, easier to Q them

;; -------------------------------------------------------------------------------------------------------------------------
;; indents config
;; -------------------------------------------------------------------------------------------------------------------------
(setq-default indent-tabs-mode nil)  ;; do not insert tabs
(setq-default sgml-basic-offset 4)  ;; indent for html
(setq-default tab-width 4)   ; standard tab width
(setq-default c-basic-offset 4)  ;; standard width for c/C++
(c-set-offset 'substatement-open 0) ;; fix c/c++ indent

;; -------------------------------------------------------------------------------------------------------------------------
;; misc
;; -------------------------------------------------------------------------------------------------------------------------
(setq minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))  ;; remove annoying minibuffer prompts
(defalias 'yes-or-no-p 'y-or-n-p)  ; do a y/s  instead of yes/no

;; -------------------------------------------------------------------------------------------------------------------------
;; ftp
;; -------------------------------------------------------------------------------------------------------------------------
(setq ange-ftp-try-passive-mode t)

;; -------------------------------------------------------------------------------------------------------------------------
;; eldoc
;; -------------------------------------------------------------------------------------------------------------------------
(global-eldoc-mode)

;; -------------------------------------------------------------------------------------------------------------------------
;; nlinum
;; -------------------------------------------------------------------------------------------------------------------------
(use-package nlinum-relative
  :config
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (add-hook 'org-mode-hook 'nlinum-relative-mode)
  (setq-default nlinum-relative-redisplay-delay 0.5))

;; -------------------------------------------------------------------------------------------------------------------------
;; hl-todo
;; -------------------------------------------------------------------------------------------------------------------------
(use-package hl-todo
  :diminish global-hl-todo-mode
  :config
  (global-hl-todo-mode)
  (add-hook 'prog-mode 'hl-todo-mode))  ;; just in case

;; -------------------------------------------------------------------------------------------------------------------------
;; git gutter
;; -------------------------------------------------------------------------------------------------------------------------
(use-package git-gutter-fringe
  :init
  (use-package fringe-helper)
  (fringe-helper-define 'git-gutter-fr:added nil
    "........"
    "....X..."
    "....X..."
    "..XXXXX."
    "....X..."
    "....X..."
    "........"
    "........")
  (fringe-helper-define 'git-gutter-fr:deleted nil
    "........"
    "........"
    "........"
    ".XXXXXX."
    ".XXXXXX."
    "........"
    "........"
    "........")
  (fringe-helper-define 'git-gutter-fr:modified nil
    "........"
    ".XXXXXX."
    ".X....X."
    ".X....X."
    ".X....X."
    ".X....X."
    ".XXXXXX."
    "........")
  :config
  (setq-default git-gutter-fr:side 'right-fringe
                git-gutter:update-interval 1)
  (set-face-foreground 'git-gutter-fr:modified "DarkOrange")
  (set-face-foreground 'git-gutter-fr:added    "OliveDrab")
  (set-face-foreground 'git-gutter-fr:deleted  "firebrick"))

;; -------------------------------------------------------------------------------------------------------------------------
;; git messenger
;; -------------------------------------------------------------------------------------------------------------------------
(use-package git-messenger
  :init
  (custom-set-variables
   '(git-messenger:use-magit-popup t)
   '(git-messenger:show-detail t)))

;; -------------------------------------------------------------------------------------------------------------------------
;; ggtags
;; -------------------------------------------------------------------------------------------------------------------------
(use-package ggtags)

;; -------------------------------------------------------------------------------------------------------------------------
;; Python
;; -------------------------------------------------------------------------------------------------------------------------
(use-package elpy
  :defer 2
  :config
  (progn
    ;; Use Flycheck instead of Flymake
    (when (require 'flycheck nil t)
      (remove-hook 'elpy-modules 'elpy-module-flymake)
      (remove-hook 'elpy-modules 'elpy-module-highlight-indentation)
      (add-hook 'elpy-mode-hook 'flycheck-mode)
      (add-hook 'elpy-mode-hook 'ggtags-mode)
      (add-hook 'elpy-mode-hook 'hl-todo-mode))
    (elpy-enable)
    (setq elpy-rpc-backend "jedi")))

;; -------------------------------------------------------------------------------------------------------------------------
;; c#
;; -------------------------------------------------------------------------------------------------------------------------
(use-package omnisharp
  :config
  (defun my-csharp-mode-setup ()
    (c-set-style "ellemtel")
    (setq indent-tabs-mode nil
          c-syntactic-indentation t
          c-basic-offset 4
          truncate-lines t
          tab-width 4
          evil-shift-width 4)
    (local-set-key (kbd "C-c C-c") 'recompile))

  (add-hook 'csharp-mode-hook 'omnisharp-mode)
  (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t))

;; -------------------------------------------------------------------------------------------------------------------------
;; flycheck linter for all
;; -------------------------------------------------------------------------------------------------------------------------
(use-package flycheck
  :config
  (global-flycheck-mode)
  (setq-default flycheck-flake8-maximum-line-length 160))

;; -------------------------------------------------------------------------------------------------------------------------
;; bookmark plus
;; -------------------------------------------------------------------------------------------------------------------------
(use-package bookmark+
  :defer t)

;; -------------------------------------------------------------------------------------------------------------------------
;; org-mode and addons
;; -------------------------------------------------------------------------------------------------------------------------
(use-package org
  :config
  (use-package org-bullets)
  (setq org-log-done t
        org-startup-folded t
        ;; org dirs
        org-directory '("~/org")
        org-default-notes-file "~/org/refile.org"
        org-agenda-files '("~/org/projects/myLectures")
        ;; org keywords
        org-todo-keywords '((sequence "VERIFY(v)"
                                      "TODO(t)"
                                      "IN-PROGRESS(i)"
                                      "|"
                                      "DONE(d)"
                                      "DELEGATED(l)"
                                      "CANCELLED(c)"))
        ;; org capture
        org-refile-targets (quote ((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9)))
        org-refile-use-outline-path t
        org-capture-templates (quote (("t" "todo" entry (file "~/org/refile.org") "* TODO %?")))
        ;; org agenda
        org-agenda-span 'week)

  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; org-helm-rifle
(use-package helm-org-rifle
  :defer t)

;; org-brain
(use-package org-brain
  :defer t
  :config
  (setq org-brain-path "/home/grimscythe/org/brain/"))

;; org-export to github markdown
(use-package ox-gfm
  :defer t
  :after org)

;; org-export to impress.js
(use-package ox-impress-js
  :defer t
  :after org)

;; org-export reveal.js
(use-package ox-reveal
  :defer t
  :after org)

;; -------------------------------------------------------------------------------------------------------------------------
;; neotree
;; -------------------------------------------------------------------------------------------------------------------------
(use-package neotree
  :defer t
  :config
  (setq neo-theme 'icons  ; set fancy arrows
        neo-smart-open t ; adjust to the current buffer
        neo-window-width 30)
  (add-hook 'neo-after-create-hook
            #'(lambda (_)
                (with-current-buffer (get-buffer neo-buffer-name)
                  (make-local-variable 'auto-hscroll-mode)
                  (setq truncate-lines t
                        word-wrap nil
                        auto-hscroll-mode nil)))))

;; -------------------------------------------------------------------------------------------------------------------------
;; rainbow delimiters
;; -------------------------------------------------------------------------------------------------------------------------
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; -------------------------------------------------------------------------------------------------------------------------
;; avy
;; -------------------------------------------------------------------------------------------------------------------------
(use-package avy
  :config
  (setq-default avy-background t
                avy-timeout-seconds 2))

;; -------------------------------------------------------------------------------------------------------------------------
;; eyebrowse
;; -------------------------------------------------------------------------------------------------------------------------
(use-package eyebrowse
  :config
  (setq-default eyebrowse-wrap-around t)
  (eyebrowse-mode t))

;; -------------------------------------------------------------------------------------------------------------------------
;; ace window
;; -------------------------------------------------------------------------------------------------------------------------
(use-package ace-window
  :defer t
  :init
  (defun window-split-into-3-columns ()
    "Split the window into three columns."
    (interactive)
    (delete-other-windows)
    (split-window-horizontally)
    (split-window-horizontally)
    (balance-windows))

  (defun window-split-into-2-columns-and-a-row ()
    "Split the window into two columns and split the second column into two rows."
    (interactive)
    (delete-other-windows)
    (split-window-right)
    (other-window 1)
    (split-window-below)
    (balance-windows))

  (defun window-split-into-4 ()
    "Split the window into two columns and split the second column into two rows."
    (interactive)
    (delete-other-windows)
    (split-window-right)
    (split-window-below)
    (other-window 2)
    (split-window-below)
    (balance-windows))

  (setq-default aw-dispatch-always t
                aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  (defvar aw-dispatch-alist
    '((?x aw-delete-window " Ace - Delete Window")
      (?u aw-delete-window " Ace - Delete Window")
      (?z aw-swap-window " Ace - Swap Window")
      (?i aw-flip-window)
      (?b aw-split-window-vert " Ace - Split Vert Window")
      (?v aw-split-window-horz " Ace - Split Horz Window")
      (?r delete-other-windows " Ace - Maximize Window")
      (?o delete-other-windows)
      (?w kill-this-buffer)
      (?3 window-split-into-3-columns)
      (?2 window-split-into-2-columns-and-a-row)
      (?4 window-split-into-4)
      "List of actions for `aw-dispatch-default'.")))

;; -------------------------------------------------------------------------------------------------------------------------
;; anzu settings
;; -------------------------------------------------------------------------------------------------------------------------
(use-package anzu
  :config
  (global-anzu-mode t)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))

;; -------------------------------------------------------------------------------------------------------------------------
;; expand region
;; -------------------------------------------------------------------------------------------------------------------------
(use-package expand-region
  :defer t)

;; -------------------------------------------------------------------------------------------------------------------------
;; helm config
;; -------------------------------------------------------------------------------------------------------------------------
(use-package helm
  :config
  (helm-mode t)
  (define-key global-map [remap find-file] 'helm-find-files)
  (define-key global-map [remap occur] 'helm-occur)
  (define-key global-map [remap list-buffers] 'helm-buffers-list)
  (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
  (unless (boundp 'completion-in-region-function)
    (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))
  (setq helm-split-window-in-side-p t
        helm-echo-input-in-header-line t
        helm-move-to-line-cycle-in-source nil))

;; -------------------------------------------------------------------------------------------------------------------------
;; helm-ag
;; -------------------------------------------------------------------------------------------------------------------------
(use-package helm-ag)

;; -------------------------------------------------------------------------------------------------------------------------
;; helm-projectile
;; -------------------------------------------------------------------------------------------------------------------------
(use-package helm-projectile
  :config
  (projectile-mode)
  ;; (setq projectile-indexing-method 'native)
  (setq-default projectile-enable-caching t
                projectile-completion-system 'helm)
  (helm-projectile-on))

;; -------------------------------------------------------------------------------------------------------------------------
;; helm-swoop
;; -------------------------------------------------------------------------------------------------------------------------
(use-package helm-swoop
  :config
  (setq helm-swoop-split-with-multiple-windows t
        helm-swoop-use-fuzzy-match nil))

;; -------------------------------------------------------------------------------------------------------------------------
;; helm-dash
;; -------------------------------------------------------------------------------------------------------------------------
(use-package helm-dash
  :config
  (setq helm-dash-min-length 2
        helm-dash-enable-debugging nil)

  (defun javascript-dash-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("JavaScript")))
  (add-hook 'js2-mode-hook 'javascript-dash-doc)

  (defun java-dash-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("Java")))
  (add-hook 'java-mode-hook 'java-dash-doc)

  (defun typescript-dash-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("TypeScript")))
  (add-hook 'typescript-mode-hook 'typescript-dash-doc)

  (defun python3-dash-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("Python 3")))
  (add-hook 'python-mode-hook 'python3-dash-doc)

  (defun html-dash-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("HTML")))
  (add-hook 'web-mode-hook 'html-dash-doc)

  (defun elisp-dash-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("Emacs Lisp")))
  (add-hook 'emacs-lisp-mode-hook 'elisp-dash-doc)

  (defun css-dash-doc ()
    (interactive)
    (setq-local helm-dash-docsets '("CSS")))
  (add-hook 'css-mode-hook 'css-dash-doc))

;; -------------------------------------------------------------------------------------------------------------------------
;; helm-flx (fuzzy match)
;; -------------------------------------------------------------------------------------------------------------------------
(use-package helm-flx
  :init
  (use-package flx)
  :ensure helm
  :config
  (helm-flx-mode +1)
  (setq helm-flx-for-helm-find-files t
        helm-flx-for-helm-locate t
        helm-buffers-fuzzy-matching nil
        helm-recentf-fuzzy-match nil
        helm-semantic-fuzzy-match t
        helm-imenu-fuzzy-match t))

;; -------------------------------------------------------------------------------------------------------------------------
;; smartparens
;; -------------------------------------------------------------------------------------------------------------------------
(use-package smartparens-config
  :ensure smartparens
  :config
  (smartparens-global-mode 1))

;; -------------------------------------------------------------------------------------------------------------------------
;; js-doc
;; -------------------------------------------------------------------------------------------------------------------------
(use-package js-doc)

;; -------------------------------------------------------------------------------------------------------------------------
;; js2 mode
;; -------------------------------------------------------------------------------------------------------------------------
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . js2-mode))  ;; attach js2 mode to js files
  (add-hook 'js2-mode-hook 'hl-todo-mode)
  (add-hook 'js2-mode-hook 'auto-highlight-symbol-mode)
  (add-hook 'js2-mode-hook (lambda() (tern-mode) (company-mode))))

;; -------------------------------------------------------------------------------------------------------------------------
;; semantic mode
;; -------------------------------------------------------------------------------------------------------------------------
(semantic-mode 1)

;; -------------------------------------------------------------------------------------------------------------------------
;; auto highlight mode
;; -------------------------------------------------------------------------------------------------------------------------
(use-package auto-highlight-symbol
  :diminish auto-highlight-symbol-mode
  :config
  (global-auto-highlight-symbol-mode t)
  (add-hook 'prog-mode 'auto-highlight-symbol-mode))

;; -------------------------------------------------------------------------------------------------------------------------
;; yasnippets and autoyasnippet
;; -------------------------------------------------------------------------------------------------------------------------
(use-package yasnippet
  :defer t
  :config
  (yas-global-mode 1))

(use-package auto-yasnippet)

;; -------------------------------------------------------------------------------------------------------------------------
;; drag stuff
;; -------------------------------------------------------------------------------------------------------------------------
(use-package drag-stuff
  :defer t
  :diminish drag-stuff-mode
  :config
  (drag-stuff-global-mode 1))

;; -------------------------------------------------------------------------------------------------------------------------
;; undo tree
;; -------------------------------------------------------------------------------------------------------------------------
(use-package undo-tree
  :config
  (setq-default undo-tree-visualizer-timestamps t
                undo-tree-auto-save-history nil  ; change undo history
                undo-tree-history-directory-alist `(("." . "~/.emacs.d/undo-tree")))  ; save all undo history into a single folder
  (global-undo-tree-mode))

;; -------------------------------------------------------------------------------------------------------------------------
;; shell pop
;; -------------------------------------------------------------------------------------------------------------------------
(use-package shell-pop
  :defer t
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell)))))
   '(shell-pop-term-shell "eshell")
   '(shell-pop-universal-key "C-'")
   '(shell-pop-window-size 30)
   '(shell-pop-full-span t)
   '(shell-pop-window-position "bottom")))

;; -------------------------------------------------------------------------------------------------------------------------
;; tabbar
;; -------------------------------------------------------------------------------------------------------------------------
(use-package tabbar
  :config
  (tabbar-mode))

;; -------------------------------------------------------------------------------------------------------------------------
;; tide mode
;; -------------------------------------------------------------------------------------------------------------------------
(use-package tide
  :config
  (defun setup-tide-mode ()
    "Setup tide mode."
    (interactive)
    (tide-setup)
    (flycheck-mode +1)
    (setq flycheck-check-syntax-automatically '(save mode-enabled))
    (auto-highlight-symbol-mode)
    (eldoc-mode +1)
    (company-mode +1))

  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  ;; format options
  (setq-default tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil)))

;; -------------------------------------------------------------------------------------------------------------------------
;; web mode
;; -------------------------------------------------------------------------------------------------------------------------
(use-package web-mode
  :init
  (setq-default web-mode-enable-current-element-highlight t
                web-mode-enable-current-column-highlight t)
  :config
  ;;auto load web mode
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

  ;; hooks
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 4
          web-mode-markup-indent-offset 4
          web-mode-css-indent-offset 4
          web-mode-code-indent-offset 4
          web-mode-enable-auto-pairing t
          web-mode-enable-css-colorization t
          web-mode-auto-close-style nil)
    (auto-highlight-symbol-mode t))

  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "jsx" (file-name-extension buffer-file-name))
                (js2-mode))))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))

  (add-hook 'web-mode-hook  'my-web-mode-hook))

;; -------------------------------------------------------------------------------------------------------------------------
;; css-eldoc
;; -------------------------------------------------------------------------------------------------------------------------
(use-package css-eldoc
  :config
  (progn
    (add-hook 'css-mode-hook 'turn-on-css-eldoc)
    (add-hook 'scss-mode-hook 'turn-on-css-eldoc)))

;; -------------------------------------------------------------------------------------------------------------------------
;; web mode
;; -------------------------------------------------------------------------------------------------------------------------
(use-package emmet-mode
  :diminish emmet-mode
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'scss-mode-hook 'emmet-mode))

;; -------------------------------------------------------------------------------------------------------------------------
;; Java mode hooks and eclim
;; -------------------------------------------------------------------------------------------------------------------------
(use-package eclim
  :init
  (require 'eclimd)
  :config
  (defun my-java-mode-hook()
    (eclim-mode t)
    (hl-todo-mode)
    (setq-default help-at-pt-display-when-idle t
                  help-at-pt-timer-delay 0.1)
    (help-at-pt-set-timer))
  (add-hook 'java-mode-hook 'my-java-mode-hook))

;; (use-package meghanada
;;   :config
;;   (add-hook 'java-mode-hook
;;             (lambda ()
;;               (meghanada-mode t)
;;               (setq c-basic-offset 4)
;;               (add-hook 'before-save-hook 'meghanada-code-beautify-before-save))))
;; -------------------------------------------------------------------------------------------------------------------------
;; GDB
;; -------------------------------------------------------------------------------------------------------------------------
(setq-default gdb-many-windows t)

;; -------------------------------------------------------------------------------------------------------------------------
;; magit
;; -------------------------------------------------------------------------------------------------------------------------
(use-package magit)

;; -------------------------------------------------------------------------------------------------------------------------
;; magit
;; -------------------------------------------------------------------------------------------------------------------------
(use-package git-timemachine)

;; -------------------------------------------------------------------------------------------------------------------------
;; sunrise commander
;; -------------------------------------------------------------------------------------------------------------------------
(use-package sunrise-commander)
(use-package sunrise-x-loop
  :after sunrise-commander)

;; -------------------------------------------------------------------------------------------------------------------------
;; dired extensions and settings
;; -------------------------------------------------------------------------------------------------------------------------
(put 'dired-find-alternate-file 'disabled nil)  ;; use single window
(setq dired-dwim-target t  ;; dired copy to other pane
      dired-auto-revert-buffer t
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-omit-verbose nil)  ;; dired refresh on change
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; dired async
(dired-async-mode)

(use-package dired+
  :config
  (setq dired-listing-switches "-alh"  ;; show file sizes in kbytes, mbytes, gbytes....
        diredp-hide-details-initially-flag nil
        diredp-hide-details-propagate-flag nil)
  (diredp-toggle-find-file-reuse-dir 1))  ;; do not open additional buffers

(use-package dired-narrow)

(use-package dired-du
  :config
  (setq dired-du-size-format t))

(use-package dired-hacks-utils)

;; (use-package dired-rainbow)

(use-package dired-launch
  :config
  (dired-launch-enable)
  (setq-default dired-launch-default-launcher '("xdg-open"))
  (setf dired-launch-extensions-map nil))


(load-file '"~/.emacs.d/dired-settings.el")  ;; load file colourings for dired and setup dired omit

(add-hook 'dired-after-readin-hook (lambda () (setq truncate-partial-width-windows t
                                                    truncate-lines t)))

;; -------------------------------------------------------------------------------------------------------------------------
;; God mode and evil god-state
;; -------------------------------------------------------------------------------------------------------------------------
(use-package god-mode
  :config
  (setq god-exempt-major-modes nil
        god-exempt-predicates nil))

(use-package evil-god-state)

;; -------------------------------------------------------------------------------------------------------------------------
;; EVIL MODE
;; -------------------------------------------------------------------------------------------------------------------------
(use-package evil-leader
  :config
  (setq evil-leader/in-all-states nil
        evil-leader/no-prefix-mode-rx '("dired-mode"
                                        "org-agenda-mode"))  ;; list of modes where leader is forced in emacs mode
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

(use-package evil
  :after evil-leader
  :config
  (evil-mode 1)
  (setq-default evil-move-cursor-back nil
                evil-cross-lines t
                evil-echo-state nil)
  ;; rename states
  (evil-put-property 'evil-state-properties 'normal   :tag " NORMAL ")
  (evil-put-property 'evil-state-properties 'insert   :tag " INSERT ")
  (evil-put-property 'evil-state-properties 'visual   :tag " VISUAL ")
  (evil-put-property 'evil-state-properties 'motion   :tag " MOTION ")
  (evil-put-property 'evil-state-properties 'emacs    :tag " EMACS ")
  (evil-put-property 'evil-state-properties 'replace  :tag " REPLACE ")
  (evil-put-property 'evil-state-properties 'operator :tag " OPERTR ")
  (evil-put-property 'evil-state-properties 'god      :tag " GOD-MODE ")

  ;; force emacs state in
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (add-to-list 'evil-emacs-state-modes 'sr-mode)
  (add-to-list 'evil-emacs-state-modes 'pomidor-mode)
  (add-to-list 'evil-emacs-state-modes 'paradox-menu-mode)
  (add-to-list 'evil-emacs-state-modes 'fundamental-mode)
  ;; force evil in modes
  (add-to-list 'evil-normal-state-modes 'notmuch-hello-mode)
  (add-to-list 'evil-normal-state-modes 'notmuch-search-mode)
  (add-to-list 'evil-normal-state-modes 'notmuch-show-mode)

  (eval-after-load 'git-timemachine
    '(progn
       (evil-make-overriding-map git-timemachine-mode-map 'normal)
       (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))  ;; git-timemachine, switch off evil
  )

(use-package evil-anzu
  :after evil)

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-matchit
  :after evil
  :config
  (global-evil-matchit-mode 1))

(use-package evil-visualstar
  :after evil
  :config
  (global-evil-visualstar-mode))

(use-package evil-args
  :defer t
  :after evil)

(use-package evil-magit
  :after evil)

(use-package evil-org
  :after evil
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (evil-org-set-key-theme '(navigation insert textobjects additional shift todo heading)))

(use-package evil-mc
  :after evil
  :config
  (setq evil-mc-undo-cursors-on-keyboard-quit t)
  (add-hook 'evil-mc-before-cursors-created (lambda () (setq-default evil-move-cursor-back t)))
  (add-hook 'evil-mc-after-cursors-deleted (lambda () (setq-default evil-move-cursor-back nil)))
  (advice-add 'helm-swoop--edit :after #'evil-mc-mode)
  (advice-add 'helm-ag--edit :after #'evil-mc-mode)
  (global-evil-mc-mode 1))

(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

(use-package evil-goggles
  :config
  (setq evil-goggles-duration 0.050)
  (evil-goggles-mode)
  (custom-set-faces
   '(evil-goggles-default-face ((t (:inherit 'bmkp-no-local))))
   '(evil-goggles-delete-face ((t (:inherit 'bmkp-su-or-sudo))))
   '(evil-goggles-paste-face ((t (:inherit 'bmkp-sequence))))
   '(evil-goggles-yank-face ((t (:inherit 'bmkp-non-file))))))

(use-package evil-nerd-commenter
  :defer t)

(use-package evil-snipe
  :diminish evil-snipe-local-mode
  :config
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'whole-buffer
        evil-snipe-smart-case t)
  (evil-snipe-mode)
  (evil-snipe-override-mode))

;; -------------------------------------------------------------------------------------------------------------------------
;; pomidor
;; -------------------------------------------------------------------------------------------------------------------------
(use-package pomidor
  :defer t
  :config
  (setq pomidor-sound-tick nil
        pomidor-sound-tack nil
        pomidor-sound-overwork nil
        alert-default-style 'libnotify))

;; -------------------------------------------------------------------------------------------------------------------------
;; dumb-jump
;; -------------------------------------------------------------------------------------------------------------------------
(use-package dumb-jump
  :defer t
  :config (setq dumb-jump-selector 'helm))

;; -------------------------------------------------------------------------------------------------------------------------
;; imenu-anywhere
;; -------------------------------------------------------------------------------------------------------------------------
(use-package imenu-anywhere
  :defer t)

;; -------------------------------------------------------------------------------------------------------------------------
;; webpaste
;; -------------------------------------------------------------------------------------------------------------------------
(use-package webpaste
  :defer t)

;; -------------------------------------------------------------------------------------------------------------------------
;; beacon
;; -------------------------------------------------------------------------------------------------------------------------
(use-package beacon
  :diminish beacon-mode
  :config
  (setq-default beacon-blink-delay nil
                beacon-color '"#FF0000"
                beacon-size 30
                beacon-blink-duration 0.2
                beacon-blink-when-point-moves-horizontally nil
                beacon-blink-when-point-moves-vertically 2)
  (beacon-mode t))

;; -------------------------------------------------------------------------------------------------------------------------
;; realgud
;; -------------------------------------------------------------------------------------------------------------------------
(use-package realgud
  :config
  (setq realgud:pdb-command-name "python -m pdb"))

;; -------------------------------------------------------------------------------------------------------------------------
;; restclient
;; -------------------------------------------------------------------------------------------------------------------------
(use-package restclient)

;; -------------------------------------------------------------------------------------------------------------------------
;; paradox
;; -------------------------------------------------------------------------------------------------------------------------
(use-package paradox)

;; -------------------------------------------------------------------------------------------------------------------------
;; hydra
;; -------------------------------------------------------------------------------------------------------------------------
(use-package hydra)

;; -------------------------------------------------------------------------------------------------------------------------
;; quickrun
;; -------------------------------------------------------------------------------------------------------------------------
(use-package quickrun)

;; -------------------------------------------------------------------------------------------------------------------------
;; web-beautify
;; -------------------------------------------------------------------------------------------------------------------------
(use-package web-beautify)

;; -------------------------------------------------------------------------------------------------------------------------
;; rainbow-mode
;; -------------------------------------------------------------------------------------------------------------------------
(use-package rainbow-mode
  :defer t)

;; -------------------------------------------------------------------------------------------------------------------------
;; zenity
;; -------------------------------------------------------------------------------------------------------------------------
(use-package zenity-color-picker)

;; -------------------------------------------------------------------------------------------------------------------------
;; markdown
;; -------------------------------------------------------------------------------------------------------------------------
(use-package markdown-mode)

;; -------------------------------------------------------------------------------------------------------------------------
;; all the icons
;; -------------------------------------------------------------------------------------------------------------------------
(use-package all-the-icons)
(use-package all-the-icons-dired
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; -------------------------------------------------------------------------------------------------------------------------
;; notmuch
;; -------------------------------------------------------------------------------------------------------------------------
(use-package notmuch
  :config
  (load-file '"~/.emacs.d/notmuch-settings.el"))

;;; Code:

;; -------------------------------------------------------------------------------------------------------------------------
;; Run custom code
;; -------------------------------------------------------------------------------------------------------------------------
;; run custom functions
(load-file '"~/.emacs.d/custom-functions.el")

;; -------------------------------------------------------------------------------------------------------------------------
;; run company config
;; -------------------------------------------------------------------------------------------------------------------------
(load-file '"~/.emacs.d/company-settings.el")

;; -------------------------------------------------------------------------------------------------------------------------
;; custom key bindings
;; -------------------------------------------------------------------------------------------------------------------------
(load-file '"~/.emacs.d/key-bindings.el")

;; -------------------------------------------------------------------------------------------------------------------------
;; load powerline
;; -------------------------------------------------------------------------------------------------------------------------
(use-package powerline)
(use-package powerline-evil
  :config
  (powerline-evil-center-color-theme)
  (setq-default powerline-default-separator (quote wave)))

;; -------------------------------------------------------------------------------------------------------------------------
;; Keep emacs Custom-settings in separate file
;; -------------------------------------------------------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; -------------------------------------------------------------------------------------------------------------------------
;; load theme
;; -------------------------------------------------------------------------------------------------------------------------
(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  (custom-set-faces
   '(tabbar-selected ((t (:inherit tabbar-default :background "#21242b" :foreground "lime green" :weight bold))))
   '(tabbar-selected-modified ((t (:inherit tabbar-selected :foreground "lime green" :underline (:color foreground-color :style wave)))))
   '(tabbar-unselected ((t (:inherit tabbar-default :foreground "#9B9FA6"))))))

;; -------------------------------------------------------------------------------------------------------------------------
;; diminish items from the modeline
;; -------------------------------------------------------------------------------------------------------------------------
(diminish 'visual-line-mode)
(diminish 'which-key-mode)
(diminish 'company-mode)
(diminish 'undo-tree-mode)
(diminish 'undo-tree-visualizer-selection-mode)
(diminish 'drag-stuff-mode)
(diminish 'helm-mode)
(diminish 'auto-highlight-symbol-mode)
(diminish 'yas-minor-mode)
(diminish 'projectile-mode)
(diminish 'anzu-mode)
(diminish 'subword-mode)
(diminish 'flycheck-mode)
(diminish 'smartparens-mode)
(diminish 'linum-relative-mode)
(diminish 'nlinum-relative-mode)
(diminish 'auto-revert-mode)
(diminish 'dired-omit-mode)
(diminish 'all-the-icons-dired-mode)
(diminish 'dired-launch-mode)
(diminish 'tern-mode)
(diminish 'rainbow-mode)
(diminish 'evil-mc-mode)
(diminish 'evil-org-mode)
(diminish 'evil-goggles-mode)
(add-hook 'evil-god-state-entry-hook (lambda () (diminish 'god-local-mode)))
(add-hook 'evil-god-state-exit-hook (lambda () (diminish-undo 'god-local-mode)))

;; -------------------------------------------------------------------------------------------------------------------------
;; Key guide (which-key)
;; -------------------------------------------------------------------------------------------------------------------------
(use-package which-key
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements "SPC h" "C-h"))

;;; Commentary:
(provide 'init)
;;; init.el ends here
