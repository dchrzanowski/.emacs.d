;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; lsp mode
;; --------------------------------------------------------------------
;; (use-package eglot
;;   :ensure-system-package ((javascript-typescript-langserver . "sudo npm i -g javascript-typescript-langserver")))

(use-package lsp-mode
  :ensure-system-package ((pyls                       . "yay -S python-language-server")
                          (jdtls                      . "yay -S jdtls")
                          (gopls                      . "yay -S gopls")
                          (typescript-language-server . "yay -S typescript-language-server")
                          (ccls                       . "yay -S ccls"))
  :config
  (setq lsp-prefer-flymake nil
        lsp-log-io nil)
  ;; fix for company to force usage of completion-at-point
  (add-hook 'lsp-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-capf)))))

  ;; lang hooks
  ;; java
  (add-hook 'java-mode-hook #'lsp)
  ;; c++
  (add-hook 'c++-mode-hook #'lsp)
  ;; js
  (add-hook 'js2-mode-hook #'lsp)
  ;; typescript
  (add-hook 'typescript-mode-hook #'lsp)
  ;; go
  ;; Set up before-save hooks to format buffer and add/delete imports.
  ;; Make sure you don't have other gofmt/goimports hooks enabled.
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook #'lsp-format-buffer t t)
    (add-hook 'before-save-hook #'lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook #'lsp))

;; --------------------------------------------------------------------
;; java lsp
;; --------------------------------------------------------------------
(use-package lsp-java
  :config)

;; --------------------------------------------------------------------
;; company lsp
;; --------------------------------------------------------------------
;; (use-package company-lsp
;;   :after lsp-mode
;;   :config
;;   (setq company-candidates-cache t)
;;   (push 'company-lsp company-backends))

;; --------------------------------------------------------------------
;; lsp up
;; --------------------------------------------------------------------
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (add-hook 'lsp-ui-mode-hook #'(lambda () (progn
                                        (lsp-ui-doc-mode -1)
                                        (lsp-ui-sideline-mode 1))))
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; --------------------------------------------------------------------
;; helm-lsp
;; --------------------------------------------------------------------
(use-package helm-lsp
  :after lsp-mode)

;; --------------------------------------------------------------------
;; lsp mode
;; --------------------------------------------------------------------
(use-package dap-mode
  :after lsp-mode)

(provide 'lsp-setup)
;;; lsp-setup ends here
