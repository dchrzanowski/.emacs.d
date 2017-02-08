;;; package --- Summary
;;; Commentary:
;; -------------------------------------------------------------------------------------------------------------------------
;;; Code:
;; -------------------------------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------------------------------
;; company config
;; -------------------------------------------------------------------------------------------------------------------------

(require 'company)
(require 'ac-html-bootstrap)
(unless (assoc "Bootstrap" web-completion-data-sources)
  (setq web-completion-data-sources
        (cons (cons "Bootstrap" "/home/grimscythe/.emacs.d/elpa/ac-html-bootstrap-20160302.901/html-stuff")
              web-completion-data-sources)))
(require 'company-web-html)
(require 'company-yasnippet)
(require 'company-emacs-eclim)
(require 'company-jedi)
;;(require 'company-anaconda)

;; initialise
(add-hook 'after-init-hook 'global-company-mode)

;;quick help mode
(company-quickhelp-mode 1)

;; hook for python with anaconda and gtags
;;(add-hook 'python-mode-hook
;;          (lambda ()
;;           (set (make-local-variable 'company-backends)
;;                 '((compnay-gtags company-anaconda company-dabbrev-code company-yasnippet)))))

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
;; select the raw hook or the eclim setup
;; ------------------------------------------------------------------------------------------------
(add-hook 'java-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-semantic company-dabbrev-code company-yasnippet)))))

;; setup for eclim (java)
;;(company-emacs-eclim-setup)

;; ------------------------------------------------------------------------------------------------
;; JS2 MODE
;; ------------------------------------------------------------------------------------------------
(add-hook 'js2-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-semantic company-dabbrev-code company-yasnippet)))))


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
