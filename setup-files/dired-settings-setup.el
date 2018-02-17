;;; package --- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
(put 'dired-find-alternate-file 'disabled nil)  ;; use single window

(setq dired-dwim-target t  ;; dired copy to other pane
      dired-auto-revert-buffer t
      dired-recursive-copies 'always
      dired-recursive-deletes 'always
      dired-omit-verbose nil)  ;; dired refresh on change

;; dired async
(dired-async-mode)

(use-package dired+
  :config
  (setq dired-listing-switches "-alh"  ;; show file sizes in kbytes, mbytes, gbytes....
        diredp-hide-details-initially-flag nil
        diredp-hide-details-propagate-flag nil)
  (diredp-toggle-find-file-reuse-dir 1))  ;; do not open additional buffers

(use-package dired-narrow)

(use-package dired-du
  :config
  (setq dired-du-size-format t))

(use-package dired-hacks-utils)

(use-package dired-launch
  :config
  (dired-launch-enable)
  (setq-default dired-launch-default-launcher '("xdg-open"))
  (setf dired-launch-extensions-map nil))

;; --------------------------------------------------------------------
;;; Dired Omit
;; --------------------------------------------------------------------
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

;; --------------------------------------------------------------------
;; Launch dired from file under point
;; --------------------------------------------------------------------
(defun helm-ff-open-dired-at-point ()
  "Launch dired from file unde point."
  (interactive)
  (helm-select-nth-action 4))

;; --------------------------------------------------------------------
;; Dired sort by name, size, date...
;; --------------------------------------------------------------------
(defun xah-dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/dired_sort.html'
Version 2015-07-30"
  (interactive)
  (let ($sort-by $arg)
    (setq $sort-by (ido-completing-read "Sort by:" '( "date" "size" "name" "dir")))
    (cond
     ((equal $sort-by "name") (setq $arg "-Al --si --time-style long-iso "))
     ((equal $sort-by "date") (setq $arg "-Al --si --time-style long-iso -t"))
     ((equal $sort-by "size") (setq $arg "-Al --si --time-style long-iso -S"))
     ((equal $sort-by "dir") (setq $arg "-Al --si --time-style long-iso --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other $arg )))

;; --------------------------------------------------------------------
;; Hooks
;; --------------------------------------------------------------------
;;omit
(add-hook 'dired-mode-hook 'dired-omit-caller)
(setq dired-omit-files (concat dired-omit-files "\\|^\\..+$"))

;; truncate lines
(add-hook 'dired-after-readin-hook (lambda () (setq truncate-partial-width-windows t
                                                    truncate-lines t)))
(add-hook 'dired-mode-hook 'auto-revert-mode)

(provide 'dired-settings-setup)
;;; dired-settings-setup.el ends here
