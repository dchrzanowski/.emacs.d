;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; Global maps
;; --------------------------------------------------------------------
(define-key undo-tree-map (kbd "C-/") 'nil)

;; esc anything
(define-key evil-normal-state-map [escape] 'keyboard-quit-and-remove-evil-mc)
(define-key evil-visual-state-map [escape] 'keyboard-quit-and-remove-evil-mc)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)
(define-key evil-emacs-state-map [escape] 'evil-exit-emacs-state)

(general-emacs-define-key 'global
  ;; fix mouse
  "C-M-)" '(lambda() (interactive)(scroll-up 6))
  "C-M-(" '(lambda() (interactive)(scroll-down 6))
  "<mouse-5>" '(lambda() (interactive)(scroll-up 3))
  "<mouse-4>" '(lambda() (interactive)(scroll-down 3))
  ;; F's
  "<f2>" 'evil-mode
  "<f5>" 'quickrun
  "<f6>" '(lambda() (interactive)(find-file "~/.emacs.d/init.el"))
  "<f7>" 'pomidor
  "<f8>" 'profiler-start
  "S-<f8>" 'profiler-report
  "C-<f8>" 'profiler-stop
  ;; smartparens
  "M-S" 'sp-split-sexp
  ;; packages
  "M-[" 'package-install
  "M-]" 'paradox-list-packages
  ;; text manipulation
  "M->" 'custom-indent-region
  "M-<" 'custom-unindent-region
  "C-j" 'dchrzan/newline-and-indent-inside-of-bracket
  ;; ace
  "M-d" 'ace-window
  "C-l" 'ace-link
  ;; helm
  "M-x" 'helm-M-x
  "M-w" 'helm-mini
  "M-f" 'helm-swoop
  "M-F" 'helm-multi-swoop-all
  "C-c C-b" 'helm-bookmarks
  "C-c C-y" 'helm-show-kill-ring
  ;; avy
  "M-e" 'avy-goto-char-timer
  "M-j" 'avy-goto-word-or-subword-1
  ;; tab-line
  "<C-tab>" 'next-buffer
  "C-<iso-lefttab>" 'previous-buffer
  ;; buffer related
  "C-w" 'kill-this-buffer
  "C-a" 'mark-whole-buffer
  ;; emmet
  "M-;" 'emmet-expand-line
  ;; org
  "C-c l" 'org-store-link
  ;; company
  "TAB" 'tab-indent-or-complete
  "<tab>" 'tab-indent-or-complete
  "C-<SPC>" 'company-dabbrev-code
  ;; eacl
  "M-<RET>" 'eacl-complete-multiline
  ;; dired
  "C-;" 'dired-jump
  ;; evil-mc
  "C-S-<mouse-1>" 'evil-mc-toggle-cursor-on-click
  ;; calc
  "M-q" 'quick-calc
  "M-Q" 'calc-eval-line
  ;; insert unicode
  "M-z" 'insert-char
  ;; down/capitalize case word
  "M-C" 'downcase-dwim
  "M-c" 'capitalize-dwim
  "M-u" 'upcase-dwim
  ;; toggle line nubers
  "M-l" 'dchrzan/toggle-line-number
  "S-<return>" 'dchrzan/toggle-line-number
  ;; neotree
  "C-/" 'nil
  "C-/" 'neotree-toggle
  ;; ranger
  "C-'" 'ranger)

;; --------------------------------------------------------------------
;; Evil insert/normal/visual
;; --------------------------------------------------------------------
(general-define-key
 :keymaps '(insert normal visual)
 ;; xref
 "M-." 'xref-find-definitions
 ;; company completion
 "M-<SPC> <SPC>" 'company-complete-common
 "M-<SPC> j" 'company-complete
 "M-<SPC> y" 'hydra-yasnippet/body
 "M-<SPC> s" 'yas-expand
 "M-<SPC> g" 'company-gtags
 "M-<SPC> f" 'company-files
 "M-<SPC> a" 'aya-create
 "M-<SPC> e" 'aya-expand
 "M-<SPC> o" 'aya-open-line
 "M-<SPC> c" 'eacl-complete-line
 "M-<SPC> C" 'eacl-complete-multiline)

;; --------------------------------------------------------------------
;; Evil normal
;; --------------------------------------------------------------------
(general-define-key
 :keymaps 'normal
 ;; drag stuff
 "M-k" 'drag-stuff-up
 "M-j" 'drag-stuff-down
 ;; date changers
 "+" 'speeddating-increase
 "_" 'speeddating-decrease
 ;; evil numbers
 "=" 'evil-numbers/inc-at-pt
 "-" 'evil-numbers/dec-at-pt
 "<kp-add>" 'evil-numbers/inc-at-pt
 "<kp-subtract>" 'evil-numbers/dec-at-pt)

;; --------------------------------------------------------------------
;; Evil insert
;; --------------------------------------------------------------------
(general-define-key
 :keymaps 'insert
 "<RET>" 'evil-ret)

;; --------------------------------------------------------------------
;; Evil normal and motion
;; --------------------------------------------------------------------
(general-define-key
 :keymaps '(normal motion)
 ;; eyebrowse
 "gt" 'eyebrowse-next-window-config
 "gT" 'eyebrowse-prev-window-config
 "gc" 'eyebrowse-close-window-config
 "g." 'eyebrowse-switch-to-window-config
 ;; motion
 "j" 'evil-next-visual-line
 "k" 'evil-previous-visual-line
 ;; jump back
 "C-S-o" 'evil-jump-forward
 ;; scroll
 "C-k" (lambda () (interactive) (evil-scroll-up nil))
 "C-j" (lambda () (interactive) (evil-scroll-down nil))
 ;; args
 "L" 'evil-forward-arg
 "H" 'evil-backward-arg
 "K" 'evil-jump-out-args)

;; --------------------------------------------------------------------
;; Evil visual
;; --------------------------------------------------------------------
(general-define-key
 :keymaps 'visual
 ;; evil jump
 "x" 'evil-exchange
 "v" 'hydra-expand-region/body
 "C-S-o" 'evil-jump-forward)

;; --------------------------------------------------------------------
;; Evil motion
;; --------------------------------------------------------------------
(general-define-key
 :keymaps 'motion
 ;; evil jump
 "<escape>" 'keyboard-quit-and-remove-evil-mc)

;; --------------------------------------------------------------------
;; Evil visual normal motion
;; --------------------------------------------------------------------
(general-define-key
 :keymaps '(visual normal motion)
 ;; avy
 "<RET>" 'evil-avy-goto-word-or-subword-1
 "S-<return>" 'evil-avy-goto-char-timer
 "C-<return>" 'evil-avy-goto-char-2)

;; --------------------------------------------------------------------
;; Local maps
;; --------------------------------------------------------------------
;; flyspell
(general-define-key
 :keymaps 'flyspell-mode-map
 "C-#" 'auto-correct-and-move-forward
 "C-;" 'nil)

;; minibuffer
(general-define-key
 :keymaps 'minibuffer-local-map
 "C-r" 'evil-paste-from-register)

;; archive mode
(general-define-key
 :keymaps 'archive-mode-map
 :states 'normal
 "F" 'archive-extract-marked-to-file)

;; magit
(general-define-key
 :keymaps 'magit-mode-map
 "M-x" 'helm-M-x)

;; helm
(general-define-key
 :keymaps 'helm-map
 "M-j" 'helm-next-line
 "M-k" 'helm-previous-line
 "C-j" 'helm-next-page
 "C-k" 'helm-previous-page
 "M-h" 'helm-next-source
 "M-l" (kbd "RET")
 "TAB" 'helm-execute-persistent-action
 "<tab>" 'helm-execute-persistent-action
 "C-S-h" 'describe-key
 "M-i" 'helm-select-action)

;; helm files
(general-define-key
 :keymaps '(helm-find-files-map
            helm-read-file-map
            helm-projectile-find-file-map
            helm-generic-files-map)
 "M-l" 'helm-execute-persistent-action
 "M-h" 'helm-find-files-up-one-level
 "M-J" 'helm-ff-run-open-file-with-default-tool
 "M-i" 'helm-select-action
 "C-S-h" 'describe-key
 "TAB" 'helm-execute-persistent-action
 "<tab>" 'helm-execute-persistent-action
 "C-r" 'evil-paste-from-register)
;; helm-locate
(general-define-key
 :keymaps '(helm-locate-map)
 "M-K" '(lambda () (interactive) (helm-select-nth-action 4)))

;; helm find-files-map
(general-define-key
 :keymaps '(helm-find-files-map
            helm-projectile-find-file-map)
 "M-K" '(lambda () (interactive) (helm-select-nth-action 1)))

;; helm swoop
(general-define-key
 :keymaps 'helm-mutli-swoop-edit-map
 :states 'normal
 "C-n" 'evil-mc-make-and-goto-next-match)

;; web mode
(general-define-key
 :keymaps 'web-mode-map
 "M-;" 'emmet-expand-line)

;; emmet
(general-define-key
 :keymaps 'emmet-mode-keymap
 "C-j" 'dchrzan/newline-and-indent-inside-of-bracket)

;; cfw caldendar
(general-define-key
 :keymaps 'cfw:calendar-mode-map
 "<SPC>" 'nil
 "n" 'next-line
 "p" 'previous-line
 "o" 'cfw:show-details-command)

(general-define-key
 :keymaps 'cfw:details-mode-map
 "h" 'cfw:details-navi-prev-command
 "l" 'cfw:details-navi-next-command
 "j" 'cfw:details-navi-next-item-command
 "k" 'cfw:details-navi-prev-item-command)

;; org
(general-define-key
 :keymaps 'org-mode-map
 "C-l" 'ace-link-org
 "<C-tab>" 'next-buffer
 "C-<iso-lefttab>" 'previous-buffer
 "C-'" 'ranger
 "M-r" 'org-refile
 "<" '(lambda () (interactive)
        (if (or (region-active-p) (looking-back "^"))
            (hydra-org-template/body)
          (self-insert-command 1))))

;; org agenda
(general-define-key
 :keymaps 'org-agenda-mode-map
 :states 'motion
 "C-j" 'scroll-up-command
 "C-k" 'scroll-down-command
 "j" 'org-agenda-next-item
 "k" 'org-agenda-previous-item
 "h" 'hydra-org-agenda/body)

;; company
(general-define-key
 :keymaps 'company-active-map
 "M-j" 'company-select-next
 "M-k" 'company-select-previous
 "TAB" 'tab-indent-or-complete
 "<tab>" 'tab-indent-or-complete
 "C-g" 'company-abort)

(evil-make-intercept-map company-active-map 'insert)
(general-def company-active-map [escape] 'company-abort-and-switch-to-normal-state)

;; yasnippet
(general-define-key
 :keymaps 'yas-minor-mode-map
 "TAB" 'nil
 "<tab>" 'nil)

(general-define-key
 :keymaps 'yas-keymap
 "TAB" 'yas-next-field-or-maybe-expand
 "<tab>" 'yas-next-field-or-maybe-expand
 "C-g" 'abort-company-or-yas)

;; info mode
(general-define-key
 :keymaps 'Info-mode-map
 :states 'motion
 "SPC" 'nil
 "<SPC>" 'nil)
;; dired
(general-define-key
 :keymaps 'dired-mode-map
 :states 'normal
 "SPC" 'nil
 "C-j" 'scroll-up-command
 "C-k" 'scroll-down-command
 "j" 'dired-hacks-next-file
 "k" 'dired-hacks-previous-file
 "l" 'dired-advertised-find-file
 "h" 'dired-up-directory
 "<backspace>" '(lambda () (interactive) (find-alternate-file ".."))
 "RET" 'dired-find-alternate-file
 "TAB" 'other-window
 "<tab>" 'other-window
 "/" 'dired-narrow-fuzzy
 "i" 'dired-subtree-insert
 "I" 'dired-subtree-remove
 "C-<right>" 'dchrzan/delete-other-windows-and-split-right
 "M-s" 'xah-dired-sort
 "M-u" 'disk-usage
 "o" 'dired-hide-dotfiles-mode
 "r" 'revert-buffer
 "J" 'dired-launch-command
 "&" 'dired-start-process
 ")" 'dired-git-info-mode)

;; ivy
(general-define-key
 :keymaps 'ivy-minibuffer-map
 "M-j" 'ivy-next-line
 "M-k" 'ivy-previous-line
 "<escape>" 'minibuffer-keyboard-quit)

;; neotree
(general-define-key
 :keymaps 'neotree-mode-map
 :states 'normal
 "gj" 'neotree-select-next-sibling-node
 "gk" 'neotree-select-previous-sibling-node
 "L" 'neotree-enter-and-close-neotree
 "l" 'neotree-quick-look
 "h" 'neotree-select-up-node
 "o" 'neotree-hidden-file-toggle
 "q" 'neotree-hide
 "RET" 'neotree-enter
 "r" 'neotree-change-root
 "R" 'neo-buffer--rename-node
 "J" 'neotree-open-xdg-on-point
 "d" 'neotree-enter-ace-window)

;; quickrun
(general-define-key
 :keymaps 'quickrun--mode-map
 :states 'normal
 "q" 'quit-window)

;; pomidor
(general-define-key
 :keymaps 'pomidor-mode-map
 "b" 'pomidor-break)

;; evil args
(general-define-key
 :keymaps 'evil-inner-text-objects-map
 "a" 'evil-inner-arg)
(general-define-key
 :keymaps 'evil-outer-text-objects-map
 "a" 'evil-outer-arg)

;; image mode
(general-define-key
 :keymaps 'image-mode-map
 "Q" 'image-kill-buffer
 "j" 'image-next-file
 "k" 'image-previous-file)

;; chronos mode
(general-define-key
 :keymaps 'chronos-mode-map
 "j" 'chronos-next-line
 "k" 'chronos-previous-line)

;; pdf view
(general-define-key
 :keymaps 'pdf-view-mode-map
 "j" 'pdf-view-next-line-or-next-page
 "k" 'pdf-view-previous-line-or-previous-page
 "C-j" 'pdf-view-next-page-command
 "C-k" 'pdf-view-previous-page-command
 "[" 'pdf-view-previous-page-command
 "]" 'pdf-view-next-page-command
 "g" 'pdf-view-first-page
 "G" 'pdf-view-last-page)

;; proced
(general-define-key
 :keymaps 'proced-mode-map
 :states 'normal
 "R" 'revert-buffer)

;; prodigy
(general-define-key
 :keymaps 'prodigy-mode-map
 :states 'motion
 "R" 'prodigy-refresh
 "`" 'prodigy-display-process)
(general-define-key
 :keymaps 'prodigy-view-mode-map
 :states 'motion
 "x" 'prodigy-view-clear-buffer)

;; transmission
(general-define-key
 :keymaps 'transmission-mode-map
 "j" 'next-line
 "k" 'previous-line)

;; evil god state
(evil-define-key 'normal global-map "," 'evil-execute-in-god-state)
(evil-define-key 'god global-map [escape] 'evil-god-state-bail)

(provide 'key-bindings-setup)
;;; key-bindings-setup.el ends here
