;;; package --- Summary
;;; Code:
;; set a much higher GC collection threshold
;; (setq-default garbage-collection-messages t)
(setq gc-cons-threshold 10000000)

;; (add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 10000000)))

;; --------------------------------------------------------------------
;; initialize package repos and make sure that use-package is installed
;; --------------------------------------------------------------------
(require 'package)

(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
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

;; diminish and bind-key for use-package extra awesomeness
(use-package diminish)
(use-package bind-key)
(use-package general
  :config
  (setq general-override-states '(insert emacs hybrid normal visual motion operator replace))
  (general-override-mode))

;; --------------------------------------------------------------------
;; setup directories
;; --------------------------------------------------------------------
(defvar user-home-directory (concat (getenv "HOME") "/"))
(setq user-emacs-directory (concat user-home-directory ".emacs.d/"))

(add-to-list 'load-path (concat user-emacs-directory "setup-files/"))
(add-to-list 'load-path (concat user-emacs-directory "custom-elisp-code"))

;; --------------------------------------------------------------------
;; personal data
;; --------------------------------------------------------------------
(setq user-full-name "Damian Chrzanowski")

;; --------------------------------------------------------------------
;; load setup files
;; --------------------------------------------------------------------
;; core and utils
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
(require 'git-setup)
(require 'project-assist-setup)
(require 'helm-setup)
(require 'highlights-setup)
(require 'hydra-setup)
(require 'linting-setup)
;; (require 'notmuch-settings-setup)
(require 'package-assistants-setup)
(require 'pdf-setup)
(require 'org-setup)
(require 'shell-setup)
(require 'smartparens-setup)
(require 'tramp-setup)
(require 'yasnippet-setup)
(require 'spell-check-setup)
(require 'docker-setup)
(require 'alerts-setup)

;; languages
;; (require 'csharp-setup)
(require 'javascript-setup)
(require 'python-setup)
(require 'typescript-setup)
;; (require 'php-setup)
(require 'web-setup)
(require 'ligatures-setup)

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

;;; Commentary:
(provide 'init)
;;; init.el ends here
