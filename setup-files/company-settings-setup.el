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
  (require 'company-ispell)
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

(use-package company-jedi
  :after company)

(use-package company-restclient
  :after company
  :defer 2
  :config
  (add-to-list 'company-backends 'company-restclient))

;; --------------------------------------------------------------------
;; ORG
;; --------------------------------------------------------------------
(add-hook 'org-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(company-dabbrev company-files))))

;; --------------------------------------------------------------------
;; NXML
;; --------------------------------------------------------------------
(add-hook 'nxml-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(company-nxml))))

;; --------------------------------------------------------------------
;; LSP mode
;; --------------------------------------------------------------------
(add-hook 'lsp-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '(company-capf))))

;; --------------------------------------------------------------------
;; Fix collision between expanding yasnippets and company
;; --------------------------------------------------------------------
;; fix for yasnippets
(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (cond
   ((minibufferp)
    (minibuffer-complete))
   (t
    (indent-for-tab-command)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (progn
              (company-manual-begin)
              (if (null company-candidates)
                  (progn
                    (company-abort)
                    (indent-for-tab-command)))))))))

(defun tab-complete-or-next-field ()
  (interactive)
  (if (or (not yas/minor-mode)
          (null (do-yas-expand)))
      (if company-candidates
          (company-complete-selection)
        (if (check-expansion)
            (progn
              (company-manual-begin)
              (if (null company-candidates)
                  (progn
                    (company-abort)
                    (yas-next-field))))
          (yas-next-field)))))

(defun expand-snippet-or-complete-selection ()
  (interactive)
  (if (or (not yas/minor-mode)
          (null (do-yas-expand))
          (company-abort))
      (company-complete-selection)))

(defun abort-company-or-yas ()
  (interactive)
  (if (null company-candidates)
      (yas-abort-snippet)
    (company-abort)))

(provide 'company-settings-setup)
;;; company-settings-setup.el ends here
