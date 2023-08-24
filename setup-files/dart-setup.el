;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
(use-package dart-mode
  :config
  (add-hook 'dart-mode-hook #'(lambda () (tree-sitter-hl-mode -1))))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/opt/flutter/"))

(provide 'dart-setup)
;;; dart-setup.el ends here
