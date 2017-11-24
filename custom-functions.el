;;; package --- Summary
;;; Commentary:
;; -------------------------------------------------------------------------------------------------------------------------
;;; Code:
;; -------------------------------------------------------------------------------------------------------------------------
;; -------------------------------------------------------------------------------------------------------------------------
;; Cool new line maker  (makes a proper enter and indent)
;; ------------------------------------------------------------------------------------------------------------------------
(defun my-fancy-newline ()
  "Add two newlines and put the cursor at the right indentation.
Between them if a newline is attempted when the cursor is between
two curly braces, otherwise do a regular newline and indent"
  (interactive)
  (progn (newline-and-indent)
         (split-line)
         (indent-for-tab-command)))

;; -------------------------------------------------------------------------------------------------------------------------
;; Beautifier
;; -------------------------------------------------------------------------------------------------------------------------
(defun untabify-buffer ()
  "Remove tabs."
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  "Fix indentation."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region BEG END."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

;; -------------------------------------------------------------------------------------------------------------------------
;; Neo tree open xdg on point helpers
;; -------------------------------------------------------------------------------------------------------------------------
(defun neotree-open-xdg-on-point ()
  "Open a file under point."
  (interactive)
  (call-process "xdg-open" nil 0 nil
                (neo-buffer--get-filename-current-line)))

;; -------------------------------------------------------------------------------------------------------------------------
;; Indent/Unindent
;; -------------------------------------------------------------------------------------------------------------------------
(defun my-indent-region (N)
  "Move a region 4 spaces right (indent) N."
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N 4))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(defun my-unindent-region (N)
  "Move a region 4 spaces left (unindent) N."
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N -4))
             (setq deactivate-mark nil))
    (self-insert-command N)))

;; -------------------------------------------------------------------------------------------------------------------------
;; Show file name in the minibuffer
;; -------------------------------------------------------------------------------------------------------------------------
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

;; -------------------------------------------------------------------------------------------------------------------------
;; Always paste from zero for evil
;; -------------------------------------------------------------------------------------------------------------------------
(defun always-paste-from-j()
  "Always paste in from the zero register"
  (interactive)
  (evil-paste-after 1 ?j))

;; -------------------------------------------------------------------------------------------------------------------------
;; esc to quit minibuffer and other...
;; -------------------------------------------------------------------------------------------------------------------------
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; -------------------------------------------------------------------------------------------------------------------------
;; insert org timestamp for the usage of yasnippet
;; -------------------------------------------------------------------------------------------------------------------------
(defun yas/org-get-time-stamp (&rest args)
  "Return the string that `org-insert-time-stamp' would insert.  ARGS."
  (with-temp-buffer
    (apply #'org-insert-time-stamp args)
    (buffer-string)))

;; -------------------------------------------------------------------------------------------------------------------------
;; custom keyboard quite to assits with evil as well
;; -------------------------------------------------------------------------------------------------------------------------
(defun my-keyboard-quit()
  "Removes the evil-mc cursors first and then does a standard keyboard-quit."
  (interactive)
  (progn
    (evil-mc-make-and-goto-first-cursor)
    (evil-normal-state)
    (keyboard-quit)))

;; -------------------------------------------------------------------------------------------------------------------------
;; insert hex color
;; -------------------------------------------------------------------------------------------------------------------------
(defun insert-color-hex ()
  "Select a color and insert its hexadecimal format."
  (interactive "*")
  (let ((buf (current-buffer)))
    (list-colors-display
     nil nil `(lambda (name)
                (interactive)
                (quit-window)
                (with-current-buffer ,buf
                  (insert (apply 'color-rgb-to-hex
                                 (color-name-to-rgb name))))))))

;; -------------------------------------------------------------------------------------------------------------------------
;; Launch dired from file under point
;; -------------------------------------------------------------------------------------------------------------------------
(defun helm-ff-open-dired-at-point ()
  "Launch dired from file unde point."
  (interactive)
  (helm-select-nth-action 4))

;; -------------------------------------------------------------------------------------------------------------------------
;; Delete other windows and split right
;; -------------------------------------------------------------------------------------------------------------------------
(defun delete-other-windows-and-split-right ()
  "Delete all other windows and split right."
  (interactive)
  (delete-other-windows)
  (split-window-right))

(provide 'custom-functions)
;;; custom-functions ends here
