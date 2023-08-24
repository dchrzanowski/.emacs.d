;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; tree-sitter
;; --------------------------------------------------------------------
;; For use with Emacs 29
;; TODO: check why the highlighting is different when using built-in ts-modes vs tree-sitter-hl-mode
;; (setq treesit-language-source-alist
;;       '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
;;         (cmake      "https://github.com/uyha/tree-sitter-cmake")
;;         (css        "https://github.com/tree-sitter/tree-sitter-css")
;;         (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
;;         (go         "https://github.com/tree-sitter/tree-sitter-go")
;;         (html       "https://github.com/tree-sitter/tree-sitter-html")
;;         (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
;;         (json       "https://github.com/tree-sitter/tree-sitter-json")
;;         (make       "https://github.com/alemuller/tree-sitter-make")
;;         (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
;;         (python     "https://github.com/tree-sitter/tree-sitter-python")
;;         (toml       "https://github.com/tree-sitter/tree-sitter-toml")
;;         (tsx        "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
;;         (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
;;         (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))

(use-package tree-sitter)

(use-package tree-sitter-langs
  :after tree-sitter
  :config
  (setq tree-sitter-major-mode-language-alist (remove
                                               '(dart-mode . dart)
                                               tree-sitter-major-mode-language-alist))
  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

;; --------------------------------------------------------------------
;; hl-todo
;; --------------------------------------------------------------------
(use-package hl-todo
  :diminish global-hl-todo-mode
  :config
  (global-hl-todo-mode)
  (add-hook 'prog-mode-hook 'hl-todo-mode))  ;; just in case

;; --------------------------------------------------------------------
;; auto highlight mode
;; --------------------------------------------------------------------
(use-package highlight-thing
  :defer 2
  :diminish highlight-thing-mode
  :config
  (setq highlight-thing-case-sensitive-p t
        highlight-thing-exclude-thing-under-point nil
        highlight-thing-what-thing 'symbol
        highlight-thing-delay-seconds 0.4)
  (add-hook 'prog-mode-hook 'highlight-thing-mode))

;; --------------------------------------------------------------------
;; rainbow delimiters
;; --------------------------------------------------------------------
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

;; --------------------------------------------------------------------
;; rainbow-mode
;; --------------------------------------------------------------------
(use-package rainbow-mode
  :defer t)

;; --------------------------------------------------------------------
;; indent guide
;; --------------------------------------------------------------------
(use-package highlight-indentation
  :defer 2
  :config
  (set-face-attribute 'highlight-indentation-face nil
                      :stipple (list 7 1 (string 16)) :inherit nil :foreground "#1f232b")
  (set-face-attribute 'highlight-indentation-current-column-face nil
                      :stipple (list 7 1 (string 16)) :inherit nil :foreground "#1f232b"))

;; --------------------------------------------------------------------
;; color identifiers
;; --------------------------------------------------------------------
(use-package color-identifiers-mode
  :defer 1
  :diminish color-identifiers-mode
  :config
  (setq color-identifiers-coloring-method 'hash
        color-identifiers:num-colors '30
        color-identifiers:timer (run-with-idle-timer 1 t 'color-identifiers:refresh)
        color-identifiers:color-luminance 0.6
        color-identifiers:recoloring-delay 2
        color-identifiers:min-color-saturation 0.3
        color-identifiers:max-color-saturation 1.0)
  (global-color-identifiers-mode))

;; --------------------------------------------------------------------
;; highlight-numbers
;; --------------------------------------------------------------------
(use-package highlight-numbers
  :config
  (add-hook 'prog-mode-hook 'highlight-numbers-mode))

(provide 'highlights-setup)
;;; highlights-setup.el ends here
