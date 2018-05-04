;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
(defun c-style-ligatures ()
  "C style languages ligatures."
  (setq prettify-symbols-alist
        '(("->" . ?λ)
          ("return" . ?↲)
          ("for" . ?∀)
          ("if" . ?◆)
          ("else" . ?→)
          ("this" . ?⅄)
          ("null" . ?∅)
          ("int" . ?ℤ)
          ("double" . ?ℝ)
          ("String" . ?ℾ)
          ("&&" . ?∧)
          ("||" . ?∨)
          ("==" . ?≡)
          ("!=" . ?≠)
          (">=" . ?≥)
          ("<=" . ?≤))))

(defun js-style-ligatures ()
  "C style languages ligatures."
  (setq prettify-symbols-alist
        '(("=>" . ?λ)
          ("return" . ?↲)
          ("for" . ?∀)
          ("if" . ?◆)
          ("else" . ?▶)
          ("&&" . ?∧)
          ("||" . ?∨)
          ("==" . ?≡)
          ("!=" . ?≠)
          ("===" . ?≣)
          ("!==" . ?≢)
          (">=" . ?≥)
          ("<=" . ?≤))))

(add-hook 'java-mode-hook #'c-style-ligatures)

(setq prettify-symbols-unprettify-at-point t)
;; (global-prettify-symbols-mode)

(provide 'ligatures-setup)
;;; ligatures-setup.el ends here
