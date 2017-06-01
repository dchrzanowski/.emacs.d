;;; package --- Summary
;;; Commentary:
;; -------------------------------------------------------------------------------------------------------------------------
;;; Code:
;; -------------------------------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------------------------------
;; company config
;; -------------------------------------------------------------------------------------------------------------------------

(use-package company
  :config
  (require 'company-yasnippet)
  (require 'company-ispell)
  (add-hook 'after-init-hook 'global-company-mode))
(use-package web-completion-data)
(use-package auto-complete)
(use-package ac-html)
(use-package ac-html-bootstrap
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
  :config
  (require 'company-web-html))

(use-package company-emacs-eclim
  :config
  (setq company-emacs-eclim-ignore-case nil))

(use-package company-jedi)

(use-package company-quickhelp
  :config
  (company-quickhelp-mode 1))

;; ------------------------------------------------------------------------------------------------
;; ORG
;; ------------------------------------------------------------------------------------------------
(add-hook 'org-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-ispell company-dabbrev company-files)))))

;; ------------------------------------------------------------------------------------------------
;; PYTHON
;; ------------------------------------------------------------------------------------------------
(add-hook 'python-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-gtags company-jedi company-dabbrev company-yasnippet)))))

;; ------------------------------------------------------------------------------------------------
;; CPP
;; ------------------------------------------------------------------------------------------------
(add-hook 'c++-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-semantic company-dabbrev-code company-yasnippet)))))

;; ------------------------------------------------------------------------------------------------
;; JAVA MODE
;; select the raw-non-eclim-hook or the eclim setup
;; ------------------------------------------------------------------------------------------------
;; (add-hook 'java-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'company-backends)
;;                  '((company-semantic company-dabbrev-code company-yasnippet)))))

;; setup for eclim (java)
(company-emacs-eclim-setup)

;; ------------------------------------------------------------------------------------------------
;; JS2 MODE
;; ------------------------------------------------------------------------------------------------
;; (add-hook 'js2-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'company-backends)
;;                  '((company-semantic company-dabbrev-code company-yasnippet)))))

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


;; poor version
;; (defun check-expansion ()
;;   (save-excursion
;;     (if (looking-at "\\_>") t
;;       (backward-char 1)
;;       (if (looking-at "\\.") t
;;         (backward-char 1)
;;         (if (looking-at "->") t nil)))))

;; (defun do-yas-expand ()
;;   (let ((yas/fallback-behavior 'return-nil))
;;     (yas/expand)))

;; (defun tab-indent-or-complete ()
;;   (interactive)
;;   (if (minibufferp)
;;       (minibuffer-complete)
;;     (if (or (not yas/minor-mode)
;;             (null (do-yas-expand)))
;;         (if (check-expansion)
;;             (company-complete-common)
;;           (indent-for-tab-command)))))

;; (global-set-key [tab] 'tab-indent-or-complete)
;; (global-set-key (kbd "TAB") 'tab-indent-or-complete)

(provide 'my-company-config)
;;; my-company-config.el ends here
