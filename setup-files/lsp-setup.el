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
  :ensure-system-package ((pylsp                      . "yay -S python-lsp-server")
                          (jdtls                      . "yay -S jdtls")
                          (gopls                      . "yay -S gopls")
                          (typescript-language-server . "yay -S typescript-language-server")
                          (flutter                    . "yay -S flutter")
                          (ccls                       . "yay -S ccls"))
  :hook ((c++-mode        . lsp)
         (js2-mode        . lsp)
         (typescript-mode . lsp)
         (gdscript-mode   . lsp)
         (go-mode         . lsp))
  :config
  ;; same definition as mentioned earlier
  (advice-add 'json-parse-string :around
              (lambda (orig string &rest rest)
                (apply orig (s-replace "\\u0000" "" string)
                       rest)))

  ;; minor changes: saves excursion and uses search-forward instead of re-search-forward
  (advice-add 'json-parse-buffer :around
              (lambda (oldfn &rest args)
                (save-excursion
                  (while (search-forward "\\u0000" nil t)
                    (replace-match "" nil t)))
                (apply oldfn args)))
  (setq lsp-prefer-flymake nil
        lsp-enable-symbol-highlighting nil
        lsp-references-exclude-definition t
        lsp-signature-doc-lines 10
        lsp-headerline-breadcrumb-segments '(file symbols)
        lsp-headerline-breadcrumb-enable nil
        lsp-log-io nil
        lsp-use-plists t)

  ;; disabled lsp clients
  (add-to-list 'lsp-disabled-clients '(typescript-mode . angular-ls)))

;; --------------------------------------------------------------------
;; java lsp
;; --------------------------------------------------------------------
;; (use-package lsp-java
;;   :hook (java-mode . lsp))

;; --------------------------------------------------------------------
;; dart lsp
;; --------------------------------------------------------------------
(use-package lsp-dart
  :hook (dart-mode . lsp)
  :config
  (setq lsp-dart-flutter-widget-guides nil))

;; --------------------------------------------------------------------
;; lsp ui
;; --------------------------------------------------------------------
(use-package lsp-ui
  :after lsp-mode
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-delay 1
        lsp-ui-doc-max-height 50
        lsp-ui-doc-alignment 'frame
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-position 'bottom
        lsp-ui-sideline-diagnostic-max-lines 4)
  (add-hook 'lsp-ui-mode-hook #'(lambda () (progn
                                        (lsp-ui-doc-mode 1)
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
  :after lsp-mode
  :config
  ;; --------------------------------------------------------------------
  ;; Set up Node debugging
  ;; --------------------------------------------------------------------
  ;; The vsextension necessary that talks to DAP is now depracated
  ;; However, it can be grabbed manually from https://github.com/microsoft/vscode-node-debug2
  ;; clone the repo and run npm i && npm run package
  ;; and unzip the vsix file into ~/.emacs.d/.extension/vscode/ms-vscode.node-debug2
  ;;
  ;; TODO:
  ;; FIXME;
  ;; The new extension is https://github.com/microsoft/vscode-js-debug
  ;; Might be worthwile looking into setting this one up instead
  ;; It offers more features.
  ;; After looking into it it works out that they've built something
  ;; that doesn't comply with their own DAP protocol...
  ;; So now DAP protocol will be amended (PR in progress) to enable
  ;; multiple sessions over a single adapter to support what they've done
  ;; with vscode-js-debug which is mostly based on Chrome's Devtools debugger...
  ;; Perhaps take a look again at the progess in about a half a year...
  (require 'dap-node)
  (dap-node-setup)

  ;; show dap hydra automatically when dap hits a breakpoint
  (add-hook 'dap-stopped-hook
            (lambda (arg) (call-interactively #'dap-hydra))))

(provide 'lsp-setup)
;;; lsp-setup.el ends here
