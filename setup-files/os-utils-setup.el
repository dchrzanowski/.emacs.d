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
    :name "Landing Page"
    :cwd "/home/grimscythe/github/Quaternion-Agri/Efarmx"
    :command "docker-compose"
    :ready-message "Running on port 8080"
    :args '("up")
    :stop-signal 'int
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Livestock App"
    :cwd "/home/grimscythe/github/Quaternion-Agri/Livestock"
    :ready-message "Connected to mongo"
    :command "docker-compose"
    :args '("up")
    :stop-signal 'int
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Arable App"
    :cwd "/home/grimscythe/github/Quaternion-Agri/Arable"
    :ready-message "Connected to mongo"
    :command "docker-compose"
    :args '("up")
    :stop-signal 'int
    :kill-process-buffer-on-stop t))

(provide 'os-utils-setup)
;;; os-utils-setup ends here
