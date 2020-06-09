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

;; default launchers for certain file types
(setq dired-guess-shell-alist-user
      '(("\\.jpe?g\\'" "gimp")
        ("\\.png\\'" "gimp")
        ("\\.gif\\'" "gimp")
        ("\\.html\\'" "chromium")
        ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|ogv\\)\\'" "vlc")))

;; sort files and show sizes
;; (setq dired-listing-switches "-alhvF --group-directories-first")
(setq dired-listing-switches "-aBhl --group-directories-first")

(require 'dired-git-info)

;; async
(use-package async
  :config
  (dired-async-mode))

;; show sizes of subdirs and dirs / file
(use-package dired-du
  :config
  (setq dired-du-size-format t))

;; dired hacks
(use-package dired-hacks-utils
  :after dired
  :config
  ;; dired-hacks-utils dependent packages:
  ;; dired subtree
  (use-package dired-subtree)
  ;; dired multistage copy/move/paste
  (use-package dired-ranger)
  ;; find files quicker
  (use-package dired-narrow)
  ;; dired filters
  (use-package dired-filter
    :config
    (setq dired-filter-saved-filters '(("Video" (extension "mkv" "mp4" "avi") (omit))
                                       ("Audio" (extension "mp3" "ogg" "wave" "flac") (omit)))

          dired-filter-group-saved-groups '(("default"
                                             ("Directory" (directory))
                                             ("SQL" (extension "sql"))
                                             ("C#" (extension "cs" "csharp"))
                                             ("Lisp" (extension "lisp"))
                                             ("Vala" (extension "vala"))
                                             ("Scala" (extension "scala"))
                                             ("Rust" (extension "rs"))
                                             ("Ruby" (extension "rb"))
                                             ("R" (extension "r"))
                                             ("PHP" (extension "php"))
                                             ("Pascal" (extension "pas"))
                                             ("OCaml" (extension "ml"))
                                             ("Nim" (extension "nim"))
                                             ("Lua" (extension "lua"))
                                             ("Julia" (extension "jl"))
                                             ("CoffeeScript" (extension "coffee"))
                                             ("Clojure" (extension "clj"))
                                             ("Dart" (extension "dart"))
                                             ("D" (extension "d"))
                                             ("Elm" (extension "elm"))
                                             ("Erlang" (extension "erl"))
                                             ("Visual F#" (extension "fs"))
                                             ("Groovy" (extension "groovy"))
                                             ("Haskell" (extension "hs"))
                                             ("Go" (extension "go"))
                                             ("TypeScript" (extension "ts"))
                                             ("JavaScript" (extension "js" "jsx"))
                                             ("Java" (extension "java" "gradle" "class"))
                                             ("Gradle" (extension "gradle"))
                                             ("C" (extension "c"))
                                             ("C++" (extension "cpp" "hpp" "hh"))
                                             ("Python" (extension "py"))
                                             ("Elisp" (extension . "el"))
                                             ("Shell" (extension . "sh"))
                                             ("Markup" (extension "html" "xhtml" "iml" "ejs"))
                                             ("XML" (extension "xml" "xhtml"))
                                             ("Stylesheet" (extension "css" "less" "sass" "scss"))
                                             ("Data" (extension "dat" "data"))
                                             ("JSON" (extension "json"))
                                             ("CSON" (extension "cson"))
                                             ("Database" (extension "sqlite" "db"))
                                             ("Config" (extension "sln" "csproj" "ini" "config" "csv" "conf" "properties"))
                                             ("Doc" (extension "pdf" "doc" "docx" "odt" "ott"))
                                             ("Org" (extension . "org"))
                                             ("LaTeX" (extension "tex" "bib"))
                                             ("Markdown" (extension "md"))
                                             ("Text" (extension "txt"))
                                             ("Spreadsheet" (extension "xls" "xlsx" "ods" "ots"))
                                             ("Presentation" (extension "ppt" "pptx" "odp" "otp"))
                                             ("Video" (extension "mkv" "mp4" "avi" "mpg" "mpeg"))
                                             ("Audio" (extension "mp3" "aiff" "ogg" "wave" "wav" "flac"))
                                             ("Image" (extension "jpg" "jpeg" "png" "bmp" "gif"))
                                             ("Subtitles" (extension "srt" "sub" "sbv" "ttxt" "psb" "ssa" "ass" "svcd" "usf"))
                                             ("Archive" (extension "zip" "tar" "gz" "7z" "xz" "jar" "iso" "pac" "pak" "rar" "bz2")))))
    (add-hook 'dired-mode-hook #'dired-filter-group-mode)))

;; launch dired from point
(use-package dired-launch
  :after dired
  :config
  (dired-launch-enable)
  (setq-default dired-launch-default-launcher '("xdg-open")
                dired-launch-extensions-map '(("html" ("chromium"))
                                              ("pdf" ("evince")))))

;; colored files by type
(use-package diredful
  :after dired
  :config
  (diredful-mode 1))

;; show/hide dotfiles
(use-package dired-hide-dotfiles
  :after dired)

;; disk-usage
(use-package disk-usage
  :after dired)


;; --------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------
;; truncate lines
(add-hook 'dired-after-readin-hook (lambda () (progn
                                           (dired-hide-details-mode)
                                           (setq truncate-partial-width-windows t
                                                 truncate-lines t))))
(add-hook 'dired-mode-hook 'auto-revert-mode)

;; --------------------------------------------------------------------
;; Advices
;; --------------------------------------------------------------------
(defun remove-buffers--dired-kill-before-delete (file &rest rest)
  "Do not ask to delete a buffer associated to a file being deleted, just delete it."
  (if-let ((buf (get-file-buffer file)))
      (kill-buffer buf)
    (dolist (dired-buf (dired-buffers-for-dir file))
      (kill-buffer dired-buf))))

(advice-add 'dired-delete-file :before 'remove-buffers--dired-kill-before-delete)

(provide 'dired-settings-setup)
;;; dired-settings-setup.el ends here
