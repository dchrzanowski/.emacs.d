;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; vertico
;; --------------------------------------------------------------------
(use-package vertico
  :config
  (vertico-mode 1)
  (setq vertico-cycle t))

;; --------------------------------------------------------------------
;; orderless
;; --------------------------------------------------------------------
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;; --------------------------------------------------------------------
;; marginalia
;; --------------------------------------------------------------------
(use-package marginalia
  :config
  (marginalia-mode 1))

;; --------------------------------------------------------------------
;; prescient
;; --------------------------------------------------------------------
(use-package vertico-prescient
  :after vertico
  :config
  (setq vertico-prescient-enable-filtering nil)
  (vertico-prescient-mode 1)
  (prescient-persist-mode 1))

;; --------------------------------------------------------------------
;; embark
;; --------------------------------------------------------------------
(use-package embark
  :bind
  ("C-." . embark-act)
  :config
  (setq prefix-help-command #'embark-prefix-help-command))

;; --------------------------------------------------------------------
;; consult
;; --------------------------------------------------------------------
(use-package consult
  :config
  (setq consult-preview-key 'any)
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;; --------------------------------------------------------------------
;; embark-consult (integration glue)
;; --------------------------------------------------------------------
(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

;; --------------------------------------------------------------------
;; consult-projectile
;; --------------------------------------------------------------------
(use-package consult-projectile
  :after (consult projectile))

;; --------------------------------------------------------------------
;; wgrep (editable grep buffers, used with embark-export)
;; --------------------------------------------------------------------
(use-package wgrep
  :defer t)

;; --------------------------------------------------------------------
;; savehist (persists minibuffer history, used by vertico/consult)
;; --------------------------------------------------------------------
(use-package savehist
  :ensure nil
  :config
  (savehist-mode 1))

;; --------------------------------------------------------------------
;; enable recursive minibuffers (needed for consult-history etc.)
;; --------------------------------------------------------------------
(setq enable-recursive-minibuffers t)

(provide 'vompecc-setup)
;;; vompecc-setup.el ends here
