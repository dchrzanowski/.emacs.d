;;; package --- Summary
;;; Code:
;; Helper
(defun megabytes-to-bytes (megabytes)
  "Convert megabytes to bytes.  Express the value as MEGABYTES."
  (* megabytes 1024 1024))

;; set a much higher GC collection threshold
;; (setq-default garbage-collection-messages t)
(setq gc-cons-threshold (megabytes-to-bytes 4))
(setq read-process-output-max (megabytes-to-bytes 1))

;; change gc threshold post init (if needed)
;; (add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 10000000)))

;; --------------------------------------------------------------------
;; initialize package repos and make sure that use-package is installed
;; --------------------------------------------------------------------
(require 'package)

(setq package-enable-at-startup nil)
(setq package-native-compile t) ;; native compilation
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org"   . "https://orgmode.org/elpa/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; --------------------------------------------------------------------
;; use-package config
;; --------------------------------------------------------------------
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)
(use-package use-package-ensure-system-package)

;; --------------------------------------------------------------------
;; allow to install packages directly from git repos for use-package
;; --------------------------------------------------------------------
(use-package quelpa)
(use-package quelpa-use-package)

;; --------------------------------------------------------------------
;; diminish and bind-key for use-package extra awesomeness
;; --------------------------------------------------------------------
(use-package diminish)
(use-package bind-key)
(use-package general
  :config
  (setq general-override-states '(insert emacs hybrid normal visual motion operator replace))
  (general-override-mode))

;; --------------------------------------------------------------------
;; setup directories
;; --------------------------------------------------------------------
(defvar user-home-directory (concat (getenv "HOME") "/")
  "Home directory location.")
(setq user-emacs-directory (concat user-home-directory ".emacs.d/"))

(add-to-list 'load-path (concat user-emacs-directory "setup-files/"))
(add-to-list 'load-path (concat user-emacs-directory "custom-elisp-code"))
(add-to-list 'load-path (concat user-emacs-directory "custom-elisp-code/bookmark-plus"))

;; --------------------------------------------------------------------
;; personal data
;; --------------------------------------------------------------------
(setq user-full-name "Damian Chrzanowski")

;; --------------------------------------------------------------------
;; load setup files
;; --------------------------------------------------------------------
;; core
(require 'sane-defaults-setup)
(require 'help-setup)
(require 'custom-functions)
(require 'saved-macros)
(require 'company-settings-setup)
(require 'eldoc-setup)
(require 'navigation-setup)
(require 'bookmarks-setup)
(require 'debuggers-setup)
(require 'dired-settings-setup)
(require 'editing-assists-setup)
(require 'evil-setup)
(require 'project-assist-setup)
(require 'helm-setup)
(require 'highlights-setup)
(require 'hydra-setup)
(require 'linting-setup)
(require 'package-assistants-setup)
(require 'smartparens-setup)
(require 'yasnippet-setup)
(require 'spell-check-setup)
(require 'alerts-setup)

;; org
(require 'org-setup)

;; mail
;; (require 'mu4e-setup)

;; utils
(require 'git-setup)
(require 'shell-setup)
(require 'tramp-setup)
(require 'pdf-setup)
(require 'docker-setup)
(require 'os-utils-setup)

;; languages
;; (require 'csharp-setup)
(require 'javascript-setup)
(require 'python-setup)
(require 'typescript-setup)
(require 'go-setup)
(require 'r-setup)
(require 'dart-setup)
;; (require 'php-setup)
(require 'web-setup)
(require 'ligatures-setup)
(require 'lsp-setup)

;; keybindings
(require 'key-bindings-setup)
(require 'key-leaders-setup)
(require 'which-key-setup)

;; diminish
(require 'diminish-setup)

;; --------------------------------------------------------------------
;; emacs custom-settings file
;; --------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;; --------------------------------------------------------------------
;; theme setup
;; --------------------------------------------------------------------
(require 'theme-setup)

;; --------------------------------------------------------------------
;; Emacs server
;; --------------------------------------------------------------------
(add-hook 'after-init-hook #'server-start)

;; --------------------------------------------------------------------
;; Open goals on startup
;; --------------------------------------------------------------------
(progn (eyebrowse-switch-to-window-config 0)
       (find-file "~/GoogleDrive/org/goals.org")
       (split-window-horizontally)
       (other-window 1)
       (find-file "~/GoogleDrive/org/Calendar.org"))


;;; Commentary:
(provide 'init)
;;; init.el ends here
