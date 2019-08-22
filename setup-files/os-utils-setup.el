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

(use-package engine-mode
  :config
  (defengine amazon
    "http://www.amazon.com/s/ref=nb_sb_noss?url=search-alias%3Daps&field-keywords=%s")

  (defengine duckduckgo
    "https://duckduckgo.com/?q=%s"
    :keybinding "d")

  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s")

  (defengine google
    "http://www.google.ie/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")

  (defengine google-images
    "http://www.google.com/images?hl=en&source=hp&biw=1440&bih=795&gbv=2&aq=f&aqi=&aql=&oq=&q=%s")

  (defengine google-maps
    "http://maps.google.com/maps?q=%s"
    :docstring "Mappin' it up.")

  (defengine project-gutenberg
    "http://www.gutenberg.org/ebooks/search/?query=%s")

  (defengine rfcs
    "http://pretty-rfc.herokuapp.com/search?q=%s")

  (defengine stack-overflow
    "https://stackoverflow.com/search?q=%s")

  (defengine twitter
    "https://twitter.com/search?q=%s")

  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis.")

  (defengine wiktionary
    "https://www.wikipedia.org/search-redirect.php?family=wiktionary&language=en&go=Go&search=%s")

  (defengine wolfram-alpha
    "http://www.wolframalpha.com/input/?i=%s")

  (defengine youtube
    "http://www.youtube.com/results?aq=f&oq=&search_query=%s")

  (engine-mode t))

(provide 'os-utils-setup)
;;; os-utils-setup ends here
