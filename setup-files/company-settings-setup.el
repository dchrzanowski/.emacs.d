;;; package --- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; company config
;; --------------------------------------------------------------------
(use-package company
  :config
  (require 'company-yasnippet)
  (require 'ispell)
  (require 'company-ispell)
  (setq company-ispell-dictionary (concat user-emacs-directory "word-dict/en_GB-large_cleaned.txt"))
  (setq ispell-alternate-dictionary (concat user-emacs-directory "word-dict/en_GB-large_cleaned.txt"))
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package web-completion-data
  :after company)

(use-package ac-html-bootstrap
  :after company
  :config
  (unless (assoc "Bootstrap" web-completion-data-sources)
    (setq web-completion-data-sources
          (cons (cons "Bootstrap" "/home/grimscythe/.emacs.d/elpa/ac-html-bootstrap-20160302.901/html-stuff")
                web-completion-data-sources)))
  (unless (assoc "FontAwesome" web-completion-data-sources)
    (setq web-completion-data-sources
          (cons (cons "FontAwesome" "/home/damian/.emacs.d/elpa/ac-html-bootstrap-20160302.901/fa-html-stuff")
                web-completion-data-sources))))

(use-package company-web
  :after company
  :config
  (require 'company-web-html))

;; Not needed anymore due to lsp mode for python [29 Nov 2025 17:26:58]
;; (use-package company-jedi
;;   :after company)

(use-package company-restclient
  :after company
  :defer 2
  :config
  (add-to-list 'company-backends 'company-restclient))

;; --------------------------------------------------------------------
;; ORG
;; --------------------------------------------------------------------
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'company-backends)
;;                  '(company-ispell company-dabbrev company-files))))
(add-hook 'org-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-capf company-dabbrev company-ispell :separate)
                   company-files))))

;; --------------------------------------------------------------------
;; hledger
;; --------------------------------------------------------------------
(add-hook 'hledger-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-dabbrev :with hledger-company)))))

;; --------------------------------------------------------------------
;; NXML
;; --------------------------------------------------------------------
(add-hook 'nxml-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-nxml)))))

;; --------------------------------------------------------------------
;; LSP mode
;; --------------------------------------------------------------------
(add-hook 'lsp-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 ;; '((company-capf :with company-yasnippet)))))
                 '(company-capf))))

;; --------------------------------------------------------------------
;; Fix collision between expanding yasnippets and company
;; --------------------------------------------------------------------


(provide 'company-settings-setup)
;;; company-settings-setup.el ends here
