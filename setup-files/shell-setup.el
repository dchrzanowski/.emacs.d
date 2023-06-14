;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; shell pop
;; --------------------------------------------------------------------
(use-package eshell
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (bind-keys
               :map eshell-mode-map
               ("<return>" . eshell-send-input)
               ("<tab>" . completion-at-point)
               ("C-c M-o" . eshell/clear)))))

;; --------------------------------------------------------------------
;; Eshell syntax highlight
;; --------------------------------------------------------------------
;; FIXME: fails to native compile, but works
;; (use-package eshell-syntax-highlighting
;;   :after eshell
;;   :ensure t
;;   :init
;;   ;; Enable in all Eshell buffers.
;;   (eshell-syntax-highlighting-global-mode +1))


(use-package shell-pop
  :defer t
  :config
  (custom-set-variables
   '(shell-pop-shell-type (quote ("eshell" "*eshell*" (lambda nil (eshell shell-pop-term-shell)))))
   '(shell-pop-term-shell "eshell")
   '(shell-pop-universal-key "C-`")
   '(shell-pop-window-size 30)
   '(shell-pop-full-span t)
   '(shell-pop-window-position "bottom")))

(provide 'shell-setup)
;;; shell-setup.el ends here
