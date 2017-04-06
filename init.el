;;; package --- Summary
;; Turn off mouse interface early in startup to avoid momentary display
(scroll-bar-mode -1)
(menu-bar-mode -1)
;; No splash screen
(setq inhibit-startup-message t)

;; set standard deletion mode and enable clipboard
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)

;; Set up load path
(add-to-list 'load-path "~/.emacs.d/settings/")
(add-to-list 'load-path "~/.emacs.d/elpa/sunrise/")

;; -------------------------------------------------------------------------------------------------------------------------
;; using melpa and load missing packages
;; -------------------------------------------------------------------------------------------------------------------------
(require 'package)
;; packages list
(setq package-list '(quickrun ac-dabbrev ac-html ac-html-bootstrap ace-window ag anaphora atom-one-dark-theme auto-complete auto-highlight-symbol auto-package-update avy bookmark+ company-emacs-eclim company-jedi company-php ac-php-core company-quickhelp company-web diminish dired+ dired-narrow dired-rainbow dired-hacks-utils drag-stuff eclim elpy company evil-anzu anzu evil-args evil-god-state evil-leader evil-magit evil-matchit evil-surround evil-visualstar expand-region f find-file-in-project ggtags god-mode helm-ag helm-flx flx helm-projectile helm-swoop helm helm-core highlight highlight-indentation htmlize ivy jedi-core epc ctable concurrent js2-mode linum-relative magit git-commit magit-popup multiple-cursors neotree nlinum-relative nlinum org-bullets palette hexrgb php-mode popup pos-tip powerline-evil powerline evil goto-chg projectile python-environment deferred pyvenv rainbow-delimiters rainbow-mode rich-minority s shell-pop smartparens speed-type tabbar tide flycheck seq pkg-info epl typescript-mode undo-tree web-completion-data web-mode which-key with-editor dash async xcscope yasnippet))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)   ;; org mode specials
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;; the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

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


;; vars
;; (if (window-system) (set-frame-size (selected-frame) 130 48)) ;; set initial frame size
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))   ; create a special folder for backup files
      backup-by-copying t    ; Don't delink hardlinks
      version-control t      ; Use version numbers on backups
      delete-old-versions t  ; Automatically delete excess backups
      kept-new-versions 20   ; how many of the newest versions to keep
      kept-old-versions 5    ; and how many of the old
      )
(setq-default indent-tabs-mode nil)  ;; do not insert tabs
(setq-default sgml-basic-offset 4)  ;; indent for html
(setq-default tab-width 4)   ; standard tab width
(setq-default c-basic-offset 4)  ;; standard width for c/C++
;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
(setq minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))  ;; remove annoying minibuffer prompts

;; modes
(savehist-mode 1) ;; save history (minibuffer)
;; (global-linum-mode) ;; show line numbers
;; (linum-relative-global-mode)  ;; show as relative numbers
(require 'nlinum-relative)
(nlinum-relative-setup-evil)
(add-hook 'prog-mode-hook 'nlinum-relative-mode)
(add-hook 'org-mode-hook 'nlinum-relative-mode)
(setq-default nlinum-relative-redisplay-delay 0.5)
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
(setq-default cursor-type 'bar) ;; set the cursor to bar style
(set-cursor-color "#FF0000")  ;; set the cursor color to red
(global-auto-revert-mode t)  ;; auto refresh file when changed on disk

;; misc
(defalias 'yes-or-no-p 'y-or-n-p)  ; do a y/s  instead of yes/no

;; -------------------------------------------------------------------------------------------------------------------------
;; Python
;; -------------------------------------------------------------------------------------------------------------------------
;; anaconda-mode
;; (add-hook 'python-mode-hook 'anaconda-mode)
;;
;; jedi mode (python)
;; (add-hook 'python-mode-hook 'jedi:setup)
;; (setq jedi:complete-on-dot t)                 ; optional

;; elpy
(elpy-enable)

;; -------------------------------------------------------------------------------------------------------------------------
;; flycheck linter for all
;; -------------------------------------------------------------------------------------------------------------------------
(global-flycheck-mode)
(setq-default flycheck-flake8-maximum-line-length 160) ;; set python line lenght to 120

;; -------------------------------------------------------------------------------------------------------------------------
;; ggtags
;; -------------------------------------------------------------------------------------------------------------------------
(require 'ggtags)
(add-hook 'python-mode-hook 'ggtags-mode)   ;; add ggtags to python mode

;; -------------------------------------------------------------------------------------------------------------------------
;; bookmark plus
;; -------------------------------------------------------------------------------------------------------------------------
(require 'bookmark+)

;; -------------------------------------------------------------------------------------------------------------------------
;; org-mode
;; -------------------------------------------------------------------------------------------------------------------------
(require 'org)
(setq org-log-done t)
(setq org-agenda-files '("~/org/projects"))

(require 'org-bullets)   ;; add pretty bullets to the org mode
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; -------------------------------------------------------------------------------------------------------------------------
;;neotree
;; -------------------------------------------------------------------------------------------------------------------------
(require 'neotree)
(setq neo-theme (quote arrow))  ; set fancy arrows
(setq neo-smart-open t) ; adjust to the current buffer
(setq neo-window-width 30)

;; -------------------------------------------------------------------------------------------------------------------------
;; rainbow delimiters
;; -------------------------------------------------------------------------------------------------------------------------
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; -------------------------------------------------------------------------------------------------------------------------
;; avy
;; -------------------------------------------------------------------------------------------------------------------------
(require 'avy)
(setq-default avy-background t)

;; -------------------------------------------------------------------------------------------------------------------------
;; ace window
;; -------------------------------------------------------------------------------------------------------------------------
(setq aw-dispatch-always t)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))

(defvar aw-dispatch-alist
  '((?x aw-delete-window " Ace - Delete Window")
    (?z aw-swap-window " Ace - Swap Window")
    (?i aw-flip-window)
    (?b aw-split-window-vert " Ace - Split Vert Window")
    (?v aw-split-window-horz " Ace - Split Horz Window")
    (?r delete-other-windows " Ace - Maximize Window")
    (?o delete-other-windows)
    (?w kill-this-buffer))
  "List of actions for `aw-dispatch-default'.")

(require 'ace-window)

;; -------------------------------------------------------------------------------------------------------------------------
;; anzu settings
;; -------------------------------------------------------------------------------------------------------------------------
(require 'anzu)
(global-anzu-mode +1)
(set-face-attribute 'anzu-mode-line nil
                    :foreground "yellow" :weight 'bold)
(define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
(define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp)

;; -------------------------------------------------------------------------------------------------------------------------
;; multiple cursors
;; -------------------------------------------------------------------------------------------------------------------------
(require 'multiple-cursors)

;; -------------------------------------------------------------------------------------------------------------------------
;; expand region
;; -------------------------------------------------------------------------------------------------------------------------
(require 'expand-region)

;; -------------------------------------------------------------------------------------------------------------------------
;; helm config
;; -------------------------------------------------------------------------------------------------------------------------
(require 'helm-config)
(helm-mode 1)
(define-key global-map [remap find-file] 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

;; -------------------------------------------------------------------------------------------------------------------------
;; helm-projectile
;; -------------------------------------------------------------------------------------------------------------------------
(require 'helm-projectile)
(projectile-mode)
;; (setq projectile-indexing-method 'native)
(setq projectile-enable-caching t)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; ignore these dirs:
(add-to-list 'projectile-globally-ignored-directories "bower_components")
(add-to-list 'projectile-globally-ignored-directories "node_modules")
(add-to-list 'projectile-globally-ignored-directories "__pycache__")
(add-to-list 'projectile-globally-ignored-directories "plugins")
(add-to-list 'projectile-globally-ignored-directories "platforms")
(add-to-list 'projectile-globally-ignored-directories "fonts")
(add-to-list 'projectile-globally-ignored-files "*.jpg")
(add-to-list 'projectile-globally-ignored-files "*.bmp")
(add-to-list 'projectile-globally-ignored-files "*.png")
(add-to-list 'projectile-globally-ignored-files "*.ttf")


;; -------------------------------------------------------------------------------------------------------------------------
;; helm-swoop
;; -------------------------------------------------------------------------------------------------------------------------
(require 'helm-swoop)
(setq helm-swoop-split-with-multiple-windows t)

;; -------------------------------------------------------------------------------------------------------------------------
;; helm-flx
;; -------------------------------------------------------------------------------------------------------------------------
(helm-flx-mode +1)
(setq helm-flx-for-helm-find-files t ;; t by default
      helm-flx-for-helm-locate t) ;; nil by default

;; -------------------------------------------------------------------------------------------------------------------------
;; smartparens
;; -------------------------------------------------------------------------------------------------------------------------
(require 'smartparens-config)
(smartparens-global-mode 1)

;; -------------------------------------------------------------------------------------------------------------------------
;; hooks for languages
;; -------------------------------------------------------------------------------------------------------------------------
(add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . js2-mode))  ;; attach js2 mode to js files

;; -------------------------------------------------------------------------------------------------------------------------
;; semantic mode
;; -------------------------------------------------------------------------------------------------------------------------
(semantic-mode 1)

;; -------------------------------------------------------------------------------------------------------------------------
;; auto highlight mode
;; -------------------------------------------------------------------------------------------------------------------------
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode t)

;; -------------------------------------------------------------------------------------------------------------------------
;; load yasnippets
;; -------------------------------------------------------------------------------------------------------------------------
(require 'yasnippet)
(yas-global-mode 1)

;; -------------------------------------------------------------------------------------------------------------------------
;; drag stuff
;; -------------------------------------------------------------------------------------------------------------------------
(drag-stuff-global-mode 1)

;; -------------------------------------------------------------------------------------------------------------------------
;; undo tree
;; -------------------------------------------------------------------------------------------------------------------------
(require 'undo-tree)
;; (setq undo-tree-visualizer-diff t)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-auto-save-history nil)  ; change undo history
(setq undo-tree-history-directory-alist `(("." . "~/.emacs.d/undo-tree")))  ; save all undo history into a single folder
(global-undo-tree-mode)


;; -------------------------------------------------------------------------------------------------------------------------
;; shell pop
;; -------------------------------------------------------------------------------------------------------------------------
(require 'shell-pop)
(custom-set-variables
 '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell)))))
 '(shell-pop-term-shell "eshell")
 '(shell-pop-universal-key "C-'")
 '(shell-pop-window-size 30)
 '(shell-pop-full-span t)
 '(shell-pop-window-position "bottom"))

;; -------------------------------------------------------------------------------------------------------------------------
;; tabbar
;; -------------------------------------------------------------------------------------------------------------------------
(require 'tabbar)
(tabbar-mode)

;; -------------------------------------------------------------------------------------------------------------------------
;; tide mode
;; -------------------------------------------------------------------------------------------------------------------------
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
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))

;; -------------------------------------------------------------------------------------------------------------------------
;; web mode
;; -------------------------------------------------------------------------------------------------------------------------
;; custom settings
(setq web-mode-enable-current-element-highlight t)
(setq web-mode-enable-current-column-highlight t)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-css-colorization t)
  )
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; -------------------------------------------------------------------------------------------------------------------------
;; emacs eclim
;; -------------------------------------------------------------------------------------------------------------------------
(require 'eclim)
(add-hook 'java-mode-hook 'eclim-mode)
(require 'eclimd)

;; -------------------------------------------------------------------------------------------------------------------------
;; GDB
;; -------------------------------------------------------------------------------------------------------------------------
(setq gdb-many-windows t)

;; -------------------------------------------------------------------------------------------------------------------------
;; sunrise commander
;; -------------------------------------------------------------------------------------------------------------------------
(require 'sunrise-commander)
(require 'sunrise-x-loop)

;; -------------------------------------------------------------------------------------------------------------------------
;; dired extensions and settings
;; -------------------------------------------------------------------------------------------------------------------------
(require 'dired-rainbow)
(diredp-toggle-find-file-reuse-dir 1)  ;; do not open additional buffers

(defconst my-dired-media-files-extensions
  '("mp3" "mp4" "MP3" "MP4" "avi" "mpg" "flv" "ogg" "mkv")
  "Media files.")

(dired-rainbow-define html "#4e9a06" ("htm" "html" "xhtml"))
(dired-rainbow-define media "#ce5c00" my-dired-media-files-extensions)

; boring regexp due to lack of imagination
(dired-rainbow-define log (:inherit default
                           :italic t) ".*\\.log")

; highlight executable files, but not directories
(dired-rainbow-define-chmod executable-unix "Green" "-[rw-]+x.*")
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

;; -------------------------------------------------------------------------------------------------------------------------
;; God mode and evil god-state
;; -------------------------------------------------------------------------------------------------------------------------
(require 'god-mode)
(setq god-exempt-major-modes nil)
(setq god-exempt-predicates nil)

;; -------------------------------------------------------------------------------------------------------------------------
;; EVIL MODE
;; -------------------------------------------------------------------------------------------------------------------------
;; (setq evil-want-C-u-scroll t)

; evil
(require 'evil)
(evil-mode 1)
(setq evil-move-cursor-back nil)

; set leader options
(setq evil-leader/in-all-states nil)
(global-evil-leader-mode)
(evil-leader/set-leader "<SPC>")

; evil-anzu
(with-eval-after-load 'evil
  (require 'evil-anzu))

; evil surround
(require 'evil-surround)
(global-evil-surround-mode 1)

; evil matchit
(require 'evil-matchit)
(global-evil-matchit-mode 1)

; evil visualstar
(global-evil-visualstar-mode)

; evil args
(require 'evil-args)

; evil magit
(require 'evil-magit)

; rename states
(evil-put-property 'evil-state-properties 'normal   :tag " NORMAL ")
(evil-put-property 'evil-state-properties 'insert   :tag " INSERT ")
(evil-put-property 'evil-state-properties 'visual   :tag " VISUAL ")
(evil-put-property 'evil-state-properties 'motion   :tag " MOTION ")
(evil-put-property 'evil-state-properties 'emacs    :tag " EMACS ")
(evil-put-property 'evil-state-properties 'replace  :tag " REPLACE ")
(evil-put-property 'evil-state-properties 'operator :tag " OPERTR ")
(evil-put-property 'evil-state-properties 'god      :tag " GOD-MODE ")

(add-to-list 'evil-emacs-state-modes 'dired-mode)
(add-to-list 'evil-emacs-state-modes 'sr-mode)
(add-to-list 'evil-emacs-state-modes 'palette)

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
(require 'powerline)
(require 'powerline-evil)
(powerline-evil-center-color-theme)

;; -------------------------------------------------------------------------------------------------------------------------
;; load emacs atom theme
;; -------------------------------------------------------------------------------------------------------------------------
(load-theme 'atom-one-dark)

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
(require 'diminish)
(diminish 'visual-line-mode)
(diminish 'which-key-mode)
(diminish 'company-mode)
(diminish 'undo-tree-mode)
(diminish 'undo-tree-visualizer-selection-mode)
(diminish 'drag-stuff-mode)
(diminish 'visual-line-mode)
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
(add-hook 'evil-god-state-entry-hook (lambda () (diminish 'god-local-mode)))
(add-hook 'evil-god-state-exit-hook (lambda () (diminish-undo 'god-local-mode)))
;; -------------------------------------------------------------------------------------------------------------------------
;; Key guide (which-key)
;; -------------------------------------------------------------------------------------------------------------------------
(which-key-mode)
(which-key-add-key-based-replacements "SPC h" "C-h")

;;; Commentary:
(provide 'init)
;;; init.el ends here
