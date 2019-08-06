;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; nginx-mode
;; --------------------------------------------------------------------
(use-package nginx-mode)

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
  (add-hook 'prodigy-mode-hook #'turn-off-evil-snipe-mode)

  (prodigy-define-service
    :name "Android Emulator"
    :cwd "~/Android/Sdk"
    :command "/home/grimscythe/Android/Sdk/emulator/emulator"
    :args '("@Nexus_5X_API_28_x86")
    :stop-signal 'int
    :tags '(Android)
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "EFarmX Livestock Mobile"
    :cwd "~/github/prefarmm-mobile-app/src"
    :command "tns"
    :args '("debug" "android")
    :stop-signal 'int
    :tags '(EFarmX)
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "EFarmX Launchpad Web"
    :cwd "~/github/Quaternion-Agri/Efarmx"
    :command "docker-compose"
    :ready-message "Running on port 8080"
    :args '("up")
    :tags '(EFarmX)
    :stop-signal 'int
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "EFarmX Livestock Web"
    :cwd "~/github/Quaternion-Agri/Livestock"
    :ready-message "Connected to mongo"
    :command "docker-compose"
    :args '("up")
    :tags '(EFarmX)
    :stop-signal 'int
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "EFarmX Arable Web"
    :cwd "~/github/Quaternion-Agri/Arable"
    :ready-message "Connected to mongo"
    :command "docker-compose"
    :args '("up")
    :tags '(EFarmX)
    :stop-signal 'int
    :kill-process-buffer-on-stop t)
  )

(provide 'os-utils-setup)
;;; os-utils-setup ends here
