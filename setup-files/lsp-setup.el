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
  (setq lsp-prefer-flymake t
        lsp-headerline-breadcrumb-segments '(file symbols)
        lsp-log-io nil)
  ;; fix for company to force usage of completion-at-point
  (add-hook 'lsp-mode-hook
            (lambda ()
              (setq lsp-dart-flutter-widget-guides nil)
              (set (make-local-variable 'company-backends)
                   '((company-capf))))))

;; --------------------------------------------------------------------
;; java lsp
;; --------------------------------------------------------------------
(use-package lsp-java
  :hook (java-mode . lsp))

;; --------------------------------------------------------------------
;; dart lsp
;; --------------------------------------------------------------------
(use-package lsp-dart
  :hook (dart-mode . lsp)
  :config ())

;; --------------------------------------------------------------------
;; lsp ui
;; --------------------------------------------------------------------
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-delay 0.5)
  (add-hook 'lsp-ui-mode-hook #'(lambda () (progn
                                        (lsp-ui-doc-mode -1)
                                        (lsp-ui-sideline-mode 1))))
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; --------------------------------------------------------------------
;; helm-lsp
;; --------------------------------------------------------------------
(use-package helm-lsp
  :after lsp-mode
  :commands helm-lsp-workspace-symbol)

;; --------------------------------------------------------------------
;; lsp dap mode (debugging)
;; --------------------------------------------------------------------
(use-package dap-mode
  :after lsp-mode)

(provide 'lsp-setup)
;;; lsp-setup ends here
