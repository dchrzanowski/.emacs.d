;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; disk-usage
;; --------------------------------------------------------------------
(use-package disk-usage)

;; --------------------------------------------------------------------
;; transmission
;; --------------------------------------------------------------------
(use-package transmission)

;; --------------------------------------------------------------------
;; proced
;; --------------------------------------------------------------------
(use-package proced
  :config
  ;; truncate long lines in proced
  (add-hook 'proced-post-display-hook (lambda () (interactive)
                                        (toggle-truncate-lines 1))))

;; --------------------------------------------------------------------
;; prodigy
;; --------------------------------------------------------------------
(use-package prodigy
  :config
  (prodigy-define-service
    :name "Livestock App"
    :command "docker-compose"
    :cwd "/home/grimscythe/github/Quaternion-Agri/Livestock"
    :args '("up")
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Arable App"
    :command "docker-compose"
    :cwd "/home/grimscythe/github/Quaternion-Agri/Arable"
    :args '("up")
    :stop-signal 'sigkill
    :kill-process-buffer-on-stop t))

(provide 'os-utils-setup)
;;; os-utils-setup ends here
