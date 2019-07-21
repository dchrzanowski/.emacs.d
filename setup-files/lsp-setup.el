;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; lsp mode
;; --------------------------------------------------------------------
;; (use-package eglot
;;   :config)

(use-package lsp-mode
  :ensure-system-package ((pyls          . "yay -S python-language-server")
                          ;; (javascript-typescript-langserver . "yay -S javascript-typescript-langserver")
                          (jdtls         . "yay -S jdtls")
                          (go-langserver . "yay -S go-langserver")
                          (ccls          . "yay -S ccls"))
  :config
  (setq lsp-prefer-flymake nil
        lsp-log-io nil)
  ;; lang hooks
  (add-hook 'java-mode-hook #'lsp)
  (add-hook 'go-mode-hook #'lsp)
  (add-hook 'c++-mode-hook #'lsp)
  ;; js and ts currently disabled, very slow. tern and tide are much faster
  ;; (add-hook 'js2-mode-hook #'lsp)
  ;; (add-hook 'typescript-mode-hook #'lsp)
  (add-hook 'python-mode-hook #'lsp))

;; --------------------------------------------------------------------
;; java lsp
;; --------------------------------------------------------------------
(use-package lsp-java
  :config)

;; --------------------------------------------------------------------
;; company lsp
;; --------------------------------------------------------------------
(use-package company-lsp
  :after lsp-mode
  :config
  (setq company-candidates-cache t)
  (push 'company-lsp company-backends))

;; --------------------------------------------------------------------
;; lsp up
;; --------------------------------------------------------------------
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (add-hook 'lsp-ui-mode-hook #'(lambda () (progn
                                        (lsp-ui-doc-mode -1)
                                        (lsp-ui-sideline-mode -1))))
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
