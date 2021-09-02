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
;; password generator
;; --------------------------------------------------------------------
(use-package password-generator)

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
    :cwd "~/Android/Sdk/emulator/"
    :command "/home/grimscythe/Android/Sdk/emulator/emulator"
    :args '("@Pixel_3a_API_30_x86")
    :stop-signal 'int
    :tags '(Android)
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "Android Emulator Wipe Data"
    :cwd "~/Android/Sdk/emulator/"
    :command "/home/grimscythe/Android/Sdk/emulator/emulator"
    :args '("@Pixel_3a_API_30_x86" "-wipe-data")
    :stop-signal 'int
    :tags '(Android AndroidWipe)
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "CORE API Server"
    :cwd "~/github/idaso/core-api/"
    :sudo t
    :command "./start.sh"
    :stop-signal 'int
    :tags '(Server)
    :ready-message "Nest application successfully started"
    :kill-process-buffer-on-stop t)

  (prodigy-define-service
    :name "CORE API Website"
    :cwd "~/github/idaso/core-website"
    :command "ng"
    :args '("serve")
    :stop-signal 'int
    :tags '(Webapp)
    :ready-message "** Angular Live Development Server"
    :kill-process-buffer-on-stop t)

  ;; (prodigy-define-service
  ;;   :name "comand.ie local Angular server"
  ;;   :cwd "~/github/sri-comand-websites/comand.ie"
  ;;   :command "ng"
  ;;   :ready-message "** Angular Live Development Server"
  ;;   :args '("serve")
  ;;   :tags '(COMAND)
  ;;   :stop-signal 'int
  ;;   :kill-process-buffer-on-stop t)

  ;; (prodigy-define-service
  ;;   :name "sri.ait.ie local Angular server"
  ;;   :cwd "~/github/sri-comand-websites/sri.ie"
  ;;   :command "ng"
  ;;   :ready-message "** Angular Live Development Server"
  ;;   :args '("serve")
  ;;   :tags '(SRI)
  ;;   :stop-signal 'int
  ;;   :kill-process-buffer-on-stop t)
  )

;; --------------------------------------------------------------------
;; engine-mode
;; --------------------------------------------------------------------
(use-package engine-mode
  :config
  (defengine amazon
    "https://www.amazon.co.uk/s?k=%s&ref=nb_sb_noss_2")

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
