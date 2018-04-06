;;; package --- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
(put 'dired-find-alternate-file 'disabled nil)  ;; use single window

(setq dired-dwim-target t
      dired-auto-revert-buffer t
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-omit-verbose nil)

;; sort files and show sizes
;; (setq dired-listing-switches "-alhvF --group-directories-first")
(setq dired-listing-switches "-aBhl --group-directories-first")

;; dired async
(dired-async-mode)


;; show sizes of subdirs and dirs / file
(use-package dired-du
  :config
  (setq dired-du-size-format t))

;; dired hacks
(use-package dired-hacks-utils
  :config

  ;; find files quicker
  (use-package dired-narrow)

  ;; dired filters
  (use-package dired-filter
    :config
    (setq dired-filter-saved-filters '(
                                       ("Video" (extension "mkv" "mp4" "avi") (omit))
                                       ("Audio" (extension "mp3" "ogg" "wave" "flac") (omit)))

          dired-filter-group-saved-groups '(("default"
                                             ("Directory" (directory))
                                             ("Code" (extension "py" "cpp" "c" "java" "gradle" "js" "jsx" "ts" "go" "sql" "cs" "lisp" "vala" "scala" "rs" "rb" "r" "php" "pas" "ml" "nim" "lua" "jl" "coffee" "clj" "dart" "d" "ex" "elm" "erl" "fs" "groovy" "hh" "hs"))
                                             ("Elisp" (extension . "el"))
                                             ("Shell" (extension . "sh"))
                                             ("Markup" (extension "xml" "html" "xhtml" "iml" "ejs"))
                                             ("Stylesheet" (extension "css" "less" "sass" "scss"))
                                             ("Data" (extension "json" "dat" "data"))
                                             ("Database" (extension "sqlite" "db"))
                                             ("Config" (extension "sln" "csproj" "ini" "config" "csv" "conf" "properties"))
                                             ("Doc" (extension "pdf" "doc" "docx" "odt"))
                                             ("Org" (extension . "org"))
                                             ("LaTeX" (extension "tex" "bib"))
                                             ("Markdown" (extension "md" "txt"))
                                             ("Spreadsheet" (extension "xls" "xlsx"))
                                             ("Presentation" (extension "ppt" "pptx"))
                                             ("Video" (extension "mkv" "mp4" "avi" "mpg" "mpeg"))
                                             ("Audio" (extension "mp3" "aiff" "ogg" "wave" "wav" "flac"))
                                             ("Image" (extension "jpg" "jpeg" "png" "bmp" "gif"))
                                             ("Archive" (extension "zip" "tar" "gz" "7z" "xz" "jar" "iso" "pac" "pak" "rar" "bz2")))))
    (add-hook 'dired-mode-hook #'dired-filter-group-mode))

  ;; dired subtree
  (use-package dired-subtree)

  ;; dired multistage copy/move/paste
  (use-package dired-ranger))

;; show/hide dotfiles
(use-package dired-hide-dotfiles
  :config
  (add-hook 'dired-mode-hook #'dired-hide-dotfiles-mode))

;; launch dired from point
(use-package dired-launch
  :config
  (dired-launch-enable)
  (setq-default dired-launch-default-launcher '("xdg-open")
                dired-launch-extensions-map nil))

(use-package diredful
  :defer 1
  :config
  (diredful-mode 1))

;; --------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------
;; truncate lines
(add-hook 'dired-after-readin-hook (lambda () (progn
                                                (dired-hide-details-mode)
                                                (setq truncate-partial-width-windows t
                                                      truncate-lines t))))
(add-hook 'dired-mode-hook 'auto-revert-mode)

(provide 'dired-settings-setup)
;;; dired-settings-setup.el ends here
