;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; js-doc
;; --------------------------------------------------------------------
(use-package js-doc
  :defer 3)

;; --------------------------------------------------------------------
;; js2 mode
;; --------------------------------------------------------------------
(use-package js2-mode
  :defer 3
  :config
  (add-to-list 'auto-mode-alist `(,(rx ".js" string-end) . js2-mode))  ;; attach js2 mode to js files

  (use-package tern
    :diminish tern-mode "𝐓𝐞"
    :if (executable-find "tern"))

  (add-hook 'js2-mode-hook 'hl-todo-mode)
  (add-hook 'js2-mode-hook 'auto-highlight-symbol-mode)
  (add-hook 'js2-mode-hook (lambda() (tern-mode) (company-mode))))

;; --------------------------------------------------------------------
;; json mode
;; --------------------------------------------------------------------
(use-package json-mode
  :defer 3)

(use-package json-snatcher
  :defer 3)

(provide 'javascript-setup)
;;; javascript-setup ends here
