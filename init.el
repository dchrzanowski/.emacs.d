;;; package --- Summary

;; personal data
(setq user-full-name '"Damian Chrzanowski")
(setq user-mail-address '"pjdamian.chrzanowski@gmail.com")

;; Turn off mouse interface early in startup to avoid momentary display
(scroll-bar-mode -1)
(menu-bar-mode -1)
;; No splash screen
(setq inhibit-startup-message t)

;; set standard deletion mode and enable clipboard
(delete-selection-mode t)
(transient-mark-mode t)
(setq select-enable-clipboard t)

;; Set up load path
(add-to-list 'load-path "~/.emacs.d/settings/")

;; -------------------------------------------------------------------------------------------------------------------------
;; using melpa and load missing packages
;; -------------------------------------------------------------------------------------------------------------------------
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)
;; packages list
;; (setq package-list '(ac-dabbrev ac-html ac-html-bootstrap ace-jump-helm-line ace-jump-mode ace-window ag all-the-icons-dired all-the-icons anaphora atom-one-dark-theme auto-complete auto-highlight-symbol auto-package-update auto-yasnippet avy bookmark+ company-emacs-eclim company-jedi company-php ac-php-core company-quickhelp company-web dired+ dired-launch dired-narrow dired-rainbow dired-hacks-utils drag-stuff eclim elpy company esup evil-anzu anzu evil-args evil-god-state evil-goggles evil-magit evil-matchit evil-mc evil-org evil-leader evil-surround evil-visualstar expand-region f find-file-in-project font-lock+ ggtags git-gutter-fringe fringe-helper git-gutter git-timemachine god-mode helm-ag helm-flx flx helm-projectile helm-swoop helm helm-core highlight highlight-indentation htmlize iedit ivy jedi-core epc ctable concurrent js2-mode lice linum-relative magithub magit magit-popup git-commit neotree nlinum-relative nlinum org-bullets palette hexrgb php-mode popup pos-tip powerline-evil powerline evil goto-chg projectile python-environment deferred pyvenv quickrun rainbow-delimiters rainbow-mode rich-minority s shell-pop smartparens solarized-theme speed-type tabbar tide flycheck seq pkg-info epl typescript-mode undo-tree use-package diminish bind-key web-completion-data web-mode which-key with-editor dash async xcscope yasnippet))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("SC" . "http://joseito.republika.pl/sunrise-commander/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)   ;; org mode specials

;; the list of packages available
;; (unless package-archive-contents
;;   (package-refresh-contents))

;; the missing packages
;; (dolist (package package-list)
;;   (unless (package-installed-p package)
;;     (package-install package)))

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

;; indents config
(setq-default indent-tabs-mode nil)  ;; do not insert tabs
(setq-default sgml-basic-offset 4)  ;; indent for html
(setq-default tab-width 4)   ; standard tab width
(setq-default c-basic-offset 4)  ;; standard width for c/C++
;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))  ;; remove annoying minibuffer prompts

;; important emacs modifications
(savehist-mode 1) ;; save history (minibuffer)
(global-visual-line-mode)   ;; scroll through visual lines
(setq-default auto-window-vscroll nil) ;; remove slow on scroll
(column-number-mode t) ;; show column numbers
(when (fboundp 'winner-mode)
  (winner-mode 1))  ;; winner mode (undo/redo frames)
(c-set-offset 'substatement-open 0) ;; fix c/c++ indent
(desktop-save-mode t)  ;; save opened buffers (and window config)
(electric-indent-mode t)  ;; auto indent
(show-paren-mode t)  ;; show matching brackets
(global-hl-line-mode)  ;; highlight current line
(global-auto-revert-mode t)  ;; auto refresh file when changed on disk
(setq-default auto-revert-verbose nil)  ;; keep auto revert quiet
(setq-default help-window-select t)  ;; auto-focus help windows, easier to Q them

;; misc
(defalias 'yes-or-no-p 'y-or-n-p)  ; do a y/s  instead of yes/no

;; -------------------------------------------------------------------------------------------------------------------------
;; use-package config
;; -------------------------------------------------------------------------------------------------------------------------
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(require 'diminish)                ;; if you use :diminish
(require 'bind-key)                ;; if you use any :bind variant

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
  (setq-default git-gutter-fr:side 'right-fringe)
  (set-face-foreground 'git-gutter-fr:modified "DarkOrange")
  (set-face-foreground 'git-gutter-fr:added    "OliveDrab")
  (set-face-foreground 'git-gutter-fr:deleted  "firebrick"))

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
      (remove-hook 'elpy-mode-hook 'elpy-module-highlight-indentation)
      (add-hook 'elpy-mode-hook 'flycheck-mode)
      (add-hook 'elpy-mode-hook 'ggtags-mode))
    (elpy-enable)
    (setq elpy-rpc-backend "jedi")))

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
;; org-mode
;; -------------------------------------------------------------------------------------------------------------------------
(use-package org
  :config
  (use-package org-bullets)
  (setq org-log-done t)
  (setq org-agenda-files '("~/org/projects"))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; -------------------------------------------------------------------------------------------------------------------------
;; neotree
;; -------------------------------------------------------------------------------------------------------------------------
(use-package neotree
  :defer t
  :config
  (setq neo-theme 'arrow)  ; set fancy arrows
  (setq neo-smart-open t) ; adjust to the current buffer
  (setq neo-window-width 30))

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
  (setq-default avy-background t))

;; -------------------------------------------------------------------------------------------------------------------------
;; ace window
;; -------------------------------------------------------------------------------------------------------------------------
(use-package ace-window
  :defer t
  :init
  (defun window-split-into-3-columns ()
    "Split the window into three columns."
    (interactive)
    (split-window-horizontally)
    (split-window-horizontally)
    (balance-windows))

  (defun window-split-into-2-columns-and-a-row ()
    "Split the window into two columns and split the second column into two rows."
    (interactive)
    (split-window-right)
    (other-window 1)
    (split-window-below)
    (balance-windows))

  (setq-default aw-dispatch-always t)
  (setq-default aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

  (defvar aw-dispatch-alist
    '((?x aw-delete-window " Ace - Delete Window")
      (?z aw-swap-window " Ace - Swap Window")
      (?i aw-flip-window)
      (?b aw-split-window-vert " Ace - Split Vert Window")
      (?v aw-split-window-horz " Ace - Split Horz Window")
      (?r delete-other-windows " Ace - Maximize Window")
      (?o delete-other-windows)
      (?w kill-this-buffer)
      (?3 window-split-into-3-columns)
      (?2 window-split-into-2-columns-and-a-row))
    "List of actions for `aw-dispatch-default'."))

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
    (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point)))

;; -------------------------------------------------------------------------------------------------------------------------
;; ace jump helm line
;; -------------------------------------------------------------------------------------------------------------------------
(use-package ace-jump-helm-line
  :ensure helm
  :config
  (define-key helm-map (kbd "M-e") 'ace-jump-helm-line))

;; -------------------------------------------------------------------------------------------------------------------------
;; helm-projectile
;; -------------------------------------------------------------------------------------------------------------------------
(use-package helm-projectile
  :config
  (projectile-mode)
  ;; (setq projectile-indexing-method 'native)
  (setq-default projectile-enable-caching t)
  (setq-default projectile-completion-system 'helm)
  (helm-projectile-on))

;; -------------------------------------------------------------------------------------------------------------------------
;; helm-swoop
;; -------------------------------------------------------------------------------------------------------------------------
(use-package helm-swoop
  :config
  (setq helm-swoop-split-with-multiple-windows t))

;; -------------------------------------------------------------------------------------------------------------------------
;; helm-flx (fuzzy match)
;; -------------------------------------------------------------------------------------------------------------------------
(use-package helm-flx
  :ensure helm
  :config
  (helm-flx-mode +1)
  (setq helm-flx-for-helm-find-files t
        helm-flx-for-helm-locate t))

;; -------------------------------------------------------------------------------------------------------------------------
;; smartparens
;; -------------------------------------------------------------------------------------------------------------------------
(use-package smartparens-config
  :ensure smartparens
  :config
  (smartparens-global-mode 1))

;; -------------------------------------------------------------------------------------------------------------------------
;; hooks for languages
;; -------------------------------------------------------------------------------------------------------------------------
(use-package js2-mode
  :config
  (add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . js2-mode)))  ;; attach js2 mode to js files

;; -------------------------------------------------------------------------------------------------------------------------
;; semantic mode
;; -------------------------------------------------------------------------------------------------------------------------
(semantic-mode 1)

;; -------------------------------------------------------------------------------------------------------------------------
;; auto highlight mode
;; -------------------------------------------------------------------------------------------------------------------------
(use-package auto-highlight-symbol
  :defer t
  :diminish auto-highlight-symbol-mode
  :config
  (global-auto-highlight-symbol-mode t))

;; -------------------------------------------------------------------------------------------------------------------------
;; yasnippets and autoyasnippet
;; -------------------------------------------------------------------------------------------------------------------------
(use-package yasnippet
  :defer t
  :config
  (yas-global-mode 1)
  (use-package auto-yasnippet))

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
  ;; (setq undo-;TODO: ree-visualizer-diff t)
  (setq-default undo-tree-visualizer-timestamps t)
  (setq-default undo-tree-auto-save-history nil)  ; change undo history
  (setq-default undo-tree-history-directory-alist `(("." . "~/.emacs.d/undo-tree")))  ; save all undo history into a single folder
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
  :defer t
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
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1)
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
  (setq-default web-mode-enable-current-element-highlight t)
  (setq-default web-mode-enable-current-column-highlight t)
  :config
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-css-indent-offset 4)
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-enable-auto-pairing t)
    (setq web-mode-enable-css-colorization t))

  (add-hook 'web-mode-hook  'my-web-mode-hook))

;; -------------------------------------------------------------------------------------------------------------------------
;; emacs eclim
;; -------------------------------------------------------------------------------------------------------------------------
(use-package eclim
  :init
  (add-hook 'java-mode-hook 'eclim-mode)
  (require 'eclimd))

;; -------------------------------------------------------------------------------------------------------------------------
;; GDB
;; -------------------------------------------------------------------------------------------------------------------------
(setq-default gdb-many-windows t)

;; -------------------------------------------------------------------------------------------------------------------------
;; magit
;; -------------------------------------------------------------------------------------------------------------------------
(use-package magit)

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t))

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
(setq dired-dwim-target t)  ;; dired copy to other pane
;; put folders obove files
(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding mark."
  (mydired-sort))

;; dired async
(dired-async-mode)

;; make omit persistant and also exclude dotfiles
(defvar v-dired-omit t
  "If dired-omit-mode enabled by default.  Don't setq me.")
(defun dired-omit-switch ()
  "This function is a small enhancement for `dired-omit-mode', which will \"remember\" omit state across Dired buffers."
  (interactive)
  (if (eq v-dired-omit t)
      (setq v-dired-omit nil)
    (setq v-dired-omit t))
  (dired-omit-caller)
  (revert-buffer))

(defun dired-omit-caller ()
  (if v-dired-omit
      (setq dired-omit-mode t)
    (setq dired-omit-mode nil)))

(add-hook 'dired-mode-hook 'dired-omit-caller)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

(use-package dired+
  :config
  (setq dired-listing-switches "-alh")  ;; show file sizes in kbytes, mbytes, gbytes....
  (diredp-toggle-find-file-reuse-dir 1))  ;; do not open additional buffers

(use-package dired-narrow)
(use-package dired-hacks-utils)

(use-package dired-rainbow)
(defconst my-dired-media-files-extensions
  '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg" "mkv")
  "Media files.")
(dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
(dired-rainbow-define media "#ce5c00" my-dired-media-files-extensions)
(dired-rainbow-define log (:inherit default
                                    :italic t) ".*\\.log")
;; highlight executable files, but not directories
(dired-rainbow-define-chmod executable-unix "Green" "-[rw-]+x.*")

;; dired launch
(use-package dired-launch
  :config
  (dired-launch-enable)
  (setq-default dired-launch-default-launcher '("xdg-open")))
;; -------------------------------------------------------------------------------------------------------------------------
;; God mode and evil god-state
;; -------------------------------------------------------------------------------------------------------------------------
(use-package god-mode
  :config
  (setq god-exempt-major-modes nil)
  (setq god-exempt-predicates nil))

;; -------------------------------------------------------------------------------------------------------------------------
;; EVIL MODE
;; -------------------------------------------------------------------------------------------------------------------------
(use-package evil
  :config
  (evil-mode 1)
  (setq evil-move-cursor-back nil)
  ;; rename states
  (evil-put-property 'evil-state-properties 'normal   :tag " NORMAL ")
  (evil-put-property 'evil-state-properties 'insert   :tag " INSERT ")
  (evil-put-property 'evil-state-properties 'visual   :tag " VISUAL ")
  (evil-put-property 'evil-state-properties 'motion   :tag " MOTION ")
  (evil-put-property 'evil-state-properties 'emacs    :tag " EMACS ")
  (evil-put-property 'evil-state-properties 'replace  :tag " REPLACE ")
  (evil-put-property 'evil-state-properties 'operator :tag " OPERTR ")
  (evil-put-property 'evil-state-properties 'god      :tag " GOD-MODE ")
  ;;emacs state in
  (add-to-list 'evil-emacs-state-modes 'dired-mode)
  (add-to-list 'evil-emacs-state-modes 'sr-mode)
  (add-to-list 'evil-emacs-state-modes 'palette-mode)
  (eval-after-load 'git-timemachine
    '(progn
       (evil-make-overriding-map git-timemachine-mode-map 'normal)
       (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))  ;; git-timemachine, switch off evil
  )

(use-package evil-leader
  :after evil
  :config
  (setq evil-leader/in-all-states nil)
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

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
  :after evil)

(use-package evil-magit
  :after evil)

(use-package evil-org
  :after evil)

(use-package evil-mc
  :after evil
  :config
  (global-evil-mc-mode 1))

(use-package evil-lion
  :ensure t
  :config
  (evil-lion-mode))

(use-package evil-goggles
  :config
  (setq evil-goggles-duration 0.100)
  (evil-goggles-mode)
  (setq evil-goggles-default-face 'bmkp-no-local)
  (setq evil-goggles-faces-alist `(
                                   ( evil-delete . bmkp-su-or-sudo )
                                   ( evil-yank . bmkp-non-file )
                                   ( evil-paste-after . bmkp-sequence)
                                   ( evil-paste-before . bmkp-sequence))))
;;; Code:

;; -------------------------------------------------------------------------------------------------------------------------
;; Run custom code
;; -------------------------------------------------------------------------------------------------------------------------
;; run custom functions
(load-file '"~/.emacs.d/my-functions.el")

;; -------------------------------------------------------------------------------------------------------------------------
;; Keep emacs Custom-settings in separate file
;; -------------------------------------------------------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; -------------------------------------------------------------------------------------------------------------------------
;; load powerline
;; -------------------------------------------------------------------------------------------------------------------------
(use-package powerline)
(use-package powerline-evil
  :config
  (powerline-evil-center-color-theme)
  (setq-default powerline-default-separator (quote wave)))

;; -------------------------------------------------------------------------------------------------------------------------
;; load emacs atom theme
;; -------------------------------------------------------------------------------------------------------------------------
(use-package atom-one-dark-theme
  :config
  (load-theme 'atom-one-dark))


;; -------------------------------------------------------------------------------------------------------------------------
;; custom key bindings
;; -------------------------------------------------------------------------------------------------------------------------
(load-file '"~/.emacs.d/my-bindings.el")

;; -------------------------------------------------------------------------------------------------------------------------
;; run company config
;; -------------------------------------------------------------------------------------------------------------------------
(load-file '"~/.emacs.d/my-company-config.el")

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
(diminish 'flycheck-mode)
(diminish 'smartparens-mode)
(diminish 'linum-relative-mode)
(diminish 'nlinum-relative-mode)
(diminish 'auto-revert-mode)
(diminish 'dired-omit-mode)
(diminish 'all-the-icons-dired-mode)
(diminish 'dired-launch-mode)
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
