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
                          (flutter                    . "yay -S flutter")
                          (ccls                       . "yay -S ccls"))
  :hook ((c++-mode . lsp)
         (js2-mode . lsp)
         (typescript-mode . lsp)
         (go-mode . lsp))
  :config
  (setq lsp-prefer-flymake nil
        lsp-log-io nil)
  ;; fix for company to force usage of completion-at-point
  (add-hook 'lsp-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   '((company-capf)))))
  ;; lang hooks that don't need their own elisp packages
  ;; c++
  ;; (add-hook 'c++-mode-hook #'lsp)
  ;; js
  ;; (add-hook 'js2-mode-hook #'lsp)
  ;; typescript
  ;; (add-hook 'typescript-mode-hook #'lsp)
  ;; go
  ;; (add-hook 'go-mode-hook #'lsp)
  )

;; --------------------------------------------------------------------
;; java lsp
;; --------------------------------------------------------------------
(use-package lsp-java
  :hook (java-mode . lsp))

;; --------------------------------------------------------------------
;; dart lsp
;; --------------------------------------------------------------------
(use-package lsp-dart
  :hook (dart-mode . lsp))

;; --------------------------------------------------------------------
;; lsp ui
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
;; lsp dap mode (debugging)
;; --------------------------------------------------------------------
(use-package dap-mode
  :after lsp-mode)

(provide 'lsp-setup)
;;; lsp-setup ends here
