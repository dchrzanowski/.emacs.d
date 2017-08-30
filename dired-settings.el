;;; package --- Summary
;;; Commentary:
;; -------------------------------------------------------------------------------------------------------------------------
;;; Code:
;; -------------------------------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------------------------------
;;; Dired rainbow
;; -------------------------------------------------------------------------------------------------------------------------
;; (dired-rainbow-define video "#6951A6" ("mp4" "avi" "mpg" "mkv" "mpeg" "flv"))
;; (dired-rainbow-define sound "#800064" ("mp3" "wav" "ogg" "flac"))
;; (dired-rainbow-define images "#14D100" ("jpg" "jpeg" "png" "bmp" "gif" "tif" "tiff"))
;; (dired-rainbow-define html "#9acd32" ("htm" "html" "xhtml"))
;; (dired-rainbow-define stylesheet "#008080" ("css" "less" "scss"))
;; (dired-rainbow-define docs "#FF0000" ("pdf" "doc" "docx" "json"))
;; (dired-rainbow-define code "#1e90ff" ("py" "cpp" "c" "java" "js" "jsx" "ts" "el" "go"))
;; (dired-rainbow-define log (:inherit default
;;                                     :italic t) ".*\\.log")
;; ;; highlight executable files, but not directories
;; (dired-rainbow-define-chmod executable-unix "Green" "-[rw-]+x.*")


;; -------------------------------------------------------------------------------------------------------------------------
;;; Dired Omit
;; -------------------------------------------------------------------------------------------------------------------------
;; put folders obove files
(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding mark."
  (mydired-sort))

;; make omit persistant and also exclude dotfiles
(defvar v-dired-omit t
  "If dired-omit-mode enabled by default.  Don't setq me.")
(defun dired-omit-switch ()
  "This function is a small enhancement for `dired-omit-mode', which will \"remember\" omit state across Dired buffers."
  (interactive)
  (if (eq v-dired-omit t)
      (setq v-dired-omit nil)
    (setq v-dired-omit t))
  (dired-omit-caller)
  (revert-buffer))

(defun dired-omit-caller ()
  (if v-dired-omit
      (setq dired-omit-mode t)
    (setq dired-omit-mode nil)))

(add-hook 'dired-mode-hook 'dired-omit-caller)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$")
      dired-omit-verbose nil)

;;; dired-settings.el ends here
(provide 'dired-settings)
