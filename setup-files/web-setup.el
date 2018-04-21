;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; web mode
;; --------------------------------------------------------------------
(use-package web-mode
  :defer 3
  :init
  (setq-default web-mode-enable-current-element-highlight t
                web-mode-enable-current-column-highlight t)
  :config
  ;;auto load web mode
  (add-to-list 'auto-mode-alist '("\\.html$" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

  ;; hooks
  (defun my-web-mode-hook ()
    "Hooks for Web mode."
    (setq web-mode-markup-indent-offset 4
          web-mode-markup-indent-offset 4
          web-mode-css-indent-offset 4
          web-mode-code-indent-offset 4
          web-mode-enable-auto-pairing t
          web-mode-enable-css-colorization t
          web-mode-auto-close-style nil)
    (auto-highlight-symbol-mode t))

  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "jsx" (file-name-extension buffer-file-name))
                (js2-mode))))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))

  (add-hook 'web-mode-hook  'my-web-mode-hook))

;; --------------------------------------------------------------------
;; css-eldoc
;; --------------------------------------------------------------------
(use-package css-eldoc
  :config
  (progn
    (add-hook 'css-mode-hook 'turn-on-css-eldoc)
    (add-hook 'scss-mode-hook 'turn-on-css-eldoc)))

;; --------------------------------------------------------------------
;; emmet mode
;; --------------------------------------------------------------------
(use-package emmet-mode
  :diminish emmet-mode
  :config
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook 'emmet-mode)
  (add-hook 'scss-mode-hook 'emmet-mode))

;; --------------------------------------------------------------------
;; restclient
;; --------------------------------------------------------------------
(use-package restclient
  :defer 2)

;; --------------------------------------------------------------------
;; web-beautify
;; --------------------------------------------------------------------
(use-package web-beautify
  :ensure-system-package (js-beautify . "sudo npm i -g js-beautify")
  :defer 2)

;; --------------------------------------------------------------------
;; zenity
;; --------------------------------------------------------------------
(use-package zenity-color-picker
  :defer 2)

(provide 'web-setup)
;;; web-setup ends here
