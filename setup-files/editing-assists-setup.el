;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; anzu settings
;; --------------------------------------------------------------------
(use-package anzu
  :defer t
  :config
  (global-anzu-mode t)
  (set-face-attribute 'anzu-mode-line nil
                      :foreground "yellow" :weight 'bold)
  (define-key isearch-mode-map [remap isearch-query-replace]  #'anzu-isearch-query-replace)
  (define-key isearch-mode-map [remap isearch-query-replace-regexp] #'anzu-isearch-query-replace-regexp))

;; --------------------------------------------------------------------
;; drag stuff
;; --------------------------------------------------------------------
(use-package drag-stuff
  :defer t
  :diminish drag-stuff-mode
  :config
  (drag-stuff-global-mode 1))

;; --------------------------------------------------------------------
;; undo tree
;; --------------------------------------------------------------------
(use-package undo-tree
  :config
  (setq-default undo-tree-visualizer-timestamps t
                undo-tree-auto-save-history nil  ; change undo history
                undo-tree-history-directory-alist `(("." . "~/.emacs.d/undo-tree")))  ; save all undo history into a single folder
  (global-undo-tree-mode))

;; --------------------------------------------------------------------
;; emacs auto complete lines
;; --------------------------------------------------------------------
(use-package eacl
  :defer t
  :config
  (eval-after-load 'grep
    '(progn
       (dolist (v '("node_modules"
                    "bower_components"
                    ".sass_cache"
                    ".cache"
                    ".npm"))
         (add-to-list 'grep-find-ignored-directories v))
       (dolist (v '("*.min.js"
                    "*.bundle.js"
                    "*.min.css"
                    "*.json"
                    "*.log"))
         (add-to-list 'grep-find-ignored-files v)))))

;; --------------------------------------------------------------------
;; speed type
;; --------------------------------------------------------------------
(use-package speed-type
  :defer t)

;; --------------------------------------------------------------------
;; webpaste
;; --------------------------------------------------------------------
(use-package webpaste
  :defer t)

;; --------------------------------------------------------------------
;; darkroom
;; --------------------------------------------------------------------
(use-package darkroom
  :defer 1
  :config
  (setq darkroom-text-scale-increase 1.5))

;; --------------------------------------------------------------------
;; easily change dates
;; --------------------------------------------------------------------
(use-package speeddating
  :defer t)

;; --------------------------------------------------------------------
;; expand region
;; --------------------------------------------------------------------
(use-package expand-region
  :defer t)

;; --------------------------------------------------------------------
;; view large files
;; --------------------------------------------------------------------
(use-package vlf
  :defer 4
  :config
  (require 'vlf-setup))

;; --------------------------------------------------------------------
;; dictionary on point
;; --------------------------------------------------------------------
(use-package define-word
  :defer t)

;; --------------------------------------------------------------------
;; multifiles
;; --------------------------------------------------------------------
(use-package multifiles
  :config
  (require 'multifiles))

;; --------------------------------------------------------------------
;; electric-operator
;; --------------------------------------------------------------------
(use-package electric-operator
  :config
  (apply #'electric-operator-add-rules-for-mode 'go-mode
         (electric-operator-get-rules-for-mode 'prog-mode))
  (electric-operator-add-rules-for-mode
   'go-mode
   (cons ":=" " := ")
   (cons "<-" "<-")
   (cons "*" #'electric-operator-c-mode-*)
   (cons "&" #'electric-operator-c-mode-&)
   (cons "++" #'electric-operator-c-mode-++)
   (cons "--" #'electric-operator-c-mode---)
   (cons "/*" " /* ")
   (cons "//" " // ")
   (cons "<<" " << ")
   (cons ">>" " >> "))
  (add-hook 'python-mode-hook #'electric-operator-mode)
  (add-hook 'js2-mode-hook #'electric-operator-mode)
  (add-hook 'go-mode-hook #'electric-operator-mode)
  (add-hook 'typescript-mode-hook #'electric-operator-mode)
  (add-hook 'java-mode-hook #'electric-operator-mode)
  (add-hook 'c++-mode-hook #'electric-operator-mode)
  ;; (add-hook 'lsp-mode-hook #'electric-operator-mode)
  (add-hook 'c-mode-hook #'electric-operator-mode))

(provide 'editing-assists-setup)
;;; editing-assists-setup.el ends here
