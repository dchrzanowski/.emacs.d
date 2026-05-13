;;; package -- Summary
;;; Commentary:
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
  (setq cape-dabbrev-check-other-buffers nil)
  ;; global fallback backends
  (add-hook 'completion-at-point-functions #'cape-dabbrev t)
  (add-hook 'completion-at-point-functions #'cape-file t))

;; --------------------------------------------------------------------
;; ispell dictionary (reuse existing word-dict)
;; --------------------------------------------------------------------
(require 'ispell)
(setq ispell-alternate-dictionary
      (concat user-emacs-directory "word-dict/en_GB-large_cleaned.txt"))

;; --------------------------------------------------------------------
;; ORG: dabbrev + ispell + file
;; --------------------------------------------------------------------
(add-hook 'org-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list (cape-capf-super
                               #'cape-dabbrev
                               (cape-capf-case-fold #'cape-ispell))
                              #'cape-file))))

;; --------------------------------------------------------------------
;; hledger
;; --------------------------------------------------------------------
(add-hook 'hledger-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list #'cape-dabbrev))))

;; --------------------------------------------------------------------
;; LSP mode: capf only (lsp-mode registers its own capf)
;; --------------------------------------------------------------------
(add-hook 'lsp-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list #'lsp-completion-at-point
                              #'cape-dabbrev))))

;; --------------------------------------------------------------------
;; yasnippet cape adapter (replaces company-yasnippet)
;; --------------------------------------------------------------------
(with-eval-after-load 'yasnippet
  (add-hook 'completion-at-point-functions
            (cape-company-to-capf #'company-yasnippet) t))

;; --------------------------------------------------------------------
;; restclient cape adapter (replaces company-restclient)
;; --------------------------------------------------------------------
(with-eval-after-load 'restclient
  (add-hook 'restclient-mode-hook
            (lambda ()
              (setq-local completion-at-point-functions
                          (list (cape-company-to-capf #'company-restclient)
                                #'cape-dabbrev)))))

(provide 'corfu-cape-setup)
;;; corfu-cape-setup.el ends here
