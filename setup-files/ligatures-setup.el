;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; ("return" . ?↲)
;; ("if" . ?◆)
;; ("else" . ?▶)
;; ("this" . ?⅄)
;; ("int" . ?ℤ)
;; ("double" . ?ℝ)
;; ("String" . ?ℾ)

(defun prog-mode-style-ligatures ()
  "General languages ligatures."
  (setq prettify-symbols-alist
        '(("lambda" . ?λ)
          ("return" . ?⮐)
          ("null"   . ?∅)
          ("NULL"   . ?∅)
          ("for"    . ?∀)
          ("in"     . ?∈)
          ("&&"     . ?∧)
          ("||"     . ?∨)
          ("=="     . ?≡)
          ("==="    . ?≣)
          ("!="     . ?≠)
          (">="     . ?≥)
          ("<="     . ?≤))))

(defun rust-style-ligatures ()
  "Rust style languages ligatures."
  (push '("fn" . ?ƒ) prettify-symbols-alist))

(defun go-style-ligatures ()
  "Golang style languages ligatures."
  (push '("func" . ?ƒ) prettify-symbols-alist))

(defun java-style-ligatures ()
  "Java style languages ligatures."
  (push '("->" . ?λ) prettify-symbols-alist))

(defun python-style-ligatures ()
  "Java style languages ligatures."
  (push '("def"    . ?ƒ) prettify-symbols-alist)
  (push '("not in" . ?∉) prettify-symbols-alist))

(defun js-style-ligatures ()
  "Js style languages ligatures."
  (push '("=>"       . ?λ) prettify-symbols-alist)
  (push '("forEach"  . ?∀) prettify-symbols-alist)
  (push '("function" . ?ƒ) prettify-symbols-alist))

(add-hook 'prog-mode-hook       #'prog-mode-style-ligatures)
(add-hook 'go-mode-hook         #'go-style-ligatures)
(add-hook 'rust-mode-hook       #'rust-style-ligatures)
(add-hook 'java-mode-hook       #'java-style-ligatures)
(add-hook 'python-mode-hook     #'python-style-ligatures)
(add-hook 'js2-mode-hook        #'js-style-ligatures)
(add-hook 'typescript-mode-hook #'js-style-ligatures)

(setq prettify-symbols-unprettify-at-point t)
(global-prettify-symbols-mode)

(provide 'ligatures-setup)
;;; ligatures-setup.el ends here
