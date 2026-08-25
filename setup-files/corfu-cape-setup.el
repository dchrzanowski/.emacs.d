;;; package -- Summary
;;; Commentary:
;;; VOMPECCC
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; corfu (in-buffer completion UI)
;; --------------------------------------------------------------------
(use-package corfu
  :config
  (setq corfu-auto t
        corfu-auto-delay 0.2
        corfu-auto-prefix 2
        corfu-cycle t
        corfu-preview-current nil
        corfu-quit-no-match 'separator)
  (global-corfu-mode 1)
  ;; show documentation popup next to corfu
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode 1)
  (setq corfu-popupinfo-delay '(0.5 . 0.2)))

;; --------------------------------------------------------------------
;; corfu-prescient (frecency sorting for corfu)
;; --------------------------------------------------------------------
(use-package corfu-prescient
  :after corfu
  :config
  (setq corfu-prescient-enable-filtering nil)
  (corfu-prescient-mode 1))

;; --------------------------------------------------------------------
;; cape (completion-at-point extensions / backends)
;; --------------------------------------------------------------------
(use-package cape
  :config
  (setq cape-dabbrev-check-other-buffers t)
  ;; global fallback backends
  (add-hook 'completion-at-point-functions #'cape-dabbrev t)
  (add-hook 'completion-at-point-functions #'cape-file t))

;; --------------------------------------------------------------------
;; capf for yasnippet
;; --------------------------------------------------------------------
(use-package yasnippet-capf
  :after (cape yasnippet)
  :config
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

;; (use-package yasnippet-capf
;;   :after (cape yasnippet)
;;   :config
;;   (add-hook 'completion-at-point-functions #'yasnippet-capf t))

;; --------------------------------------------------------------------
;; ORG: dabbrev + ispell + file
;; --------------------------------------------------------------------
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list #'cape-file
                              (cape-capf-super
                               #'cape-dict
                               #'yasnippet-capf
                               #'cape-dabbrev)))))

;; --------------------------------------------------------------------
;; emacs lisp
;; --------------------------------------------------------------------
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list (cape-capf-super
                               #'elisp-completion-at-point
                               #'yasnippet-capf
                               #'cape-dabbrev)))))

;; --------------------------------------------------------------------
;; hledger
;; --------------------------------------------------------------------
(add-hook 'hledger-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list #'cape-dabbrev))))

;; --------------------------------------------------------------------
;; LSP mode: merge lsp + yasnippet + dabbrev via cape-capf-super
;; --------------------------------------------------------------------
(add-hook 'lsp-completion-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list (cape-capf-super
                               (cape-capf-properties #'lsp-completion-at-point :exclusive 'no)
                               #'yasnippet-capf
                               #'cape-dabbrev)))))

;; --------------------------------------------------------------------
;; restclient cape adapter (replaces company-restclient)
;; --------------------------------------------------------------------
;; (with-eval-after-load 'restclient
;;   (add-hook 'restclient-mode-hook
;;             (lambda ()
;;               (setq-local completion-at-point-functions
;;                           (list (cape-company-to-capf #'company-restclient)
;;                                 #'cape-dabbrev)))))

;; --------------------------------------------------------------------
;; kind-icon icons for corfu
;; --------------------------------------------------------------------
(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-blend-background t)
  (kind-icon-default-face 'corfu-default) ; only needed with blend-background
  :config
  (setq kind-icon-default-style
        '(:padding 0 :stroke 0 :margin 0 :radius 0 :height 0.9 :scale 1.0
                   :background nil))
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(provide 'corfu-cape-setup)
;;; corfu-cape-setup.el ends here
