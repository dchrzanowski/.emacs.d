;;; package --- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; Cool new line maker  (makes a proper enter and indent)
;; --------------------------------------------------------------------
(defun grim/newline-and-indent-inside-of-bracket ()
  "Add two newlines and put the cursor at the right indentation.
Between them if a newline is attempted when the cursor is between
two curly braces, otherwise do a regular newline and indent"
  (interactive)
  (progn (newline-and-indent)
         (split-line)
         (indent-for-tab-command)))

;; --------------------------------------------------------------------
;; Beautifier
;; --------------------------------------------------------------------
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

;; --------------------------------------------------------------------
;; Neo tree open xdg on point helpers
;; --------------------------------------------------------------------
(defun neotree-open-xdg-on-point ()
  "Open a file under point."
  (interactive)
  (call-process "xdg-open" nil 0 nil
                (neo-buffer--get-filename-current-line)))

;; --------------------------------------------------------------------
;; Indent/Unindent
;; --------------------------------------------------------------------
(defun custom-indent-region (N)
  "Move a region 4 spaces right (indent) N."
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N 4))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(defun custom-unindent-region (N)
  "Move a region 4 spaces left (unindent) N."
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N -4))
             (setq deactivate-mark nil))
    (self-insert-command N)))

;; --------------------------------------------------------------------
;; Show file name in the minibuffer
;; --------------------------------------------------------------------
(defun show-file-name ()
  "Show the full path file name in the minibuffer."
  (interactive)
  (message (buffer-file-name)))

;; --------------------------------------------------------------------
;; Always paste from zero for evil
;; --------------------------------------------------------------------
(defun always-paste-from-j()
  "Always paste in from the zero register"
  (interactive)
  (evil-paste-after 1 ?j))

;; --------------------------------------------------------------------
;; esc to quit minibuffer and other...
;; --------------------------------------------------------------------
(defun minibuffer-keyboard-quit ()
  "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
  (interactive)
  (if (and delete-selection-mode transient-mark-mode mark-active)
      (setq deactivate-mark  t)
    (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
    (abort-recursive-edit)))

;; --------------------------------------------------------------------
;; insert org timestamp for the usage of yasnippet
;; --------------------------------------------------------------------
(defun yas/org-get-time-stamp (&rest args)
  "Return the string that `org-insert-time-stamp' would insert.  ARGS."
  (with-temp-buffer
    (apply #'org-insert-time-stamp args)
    (buffer-string)))

;; --------------------------------------------------------------------
;; custom keyboard quite to assits with evil as well
;; --------------------------------------------------------------------
(defun keyboard-quit-and-remove-evil-mc ()
  "Removes the evil-mc cursors first and then does a standard keyboard-quit."
  (interactive)
  (when evil-mc-cursor-state
    (evil-mc-make-and-goto-first-cursor))
  (evil-normal-state)
  (keyboard-quit))

;; --------------------------------------------------------------------
;; insert hex color
;; --------------------------------------------------------------------
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

;; --------------------------------------------------------------------
;; Org-agenda full screen
;; --------------------------------------------------------------------
(defun org-agenda-list-and-delete-other-windows ()
  "Display org agenda in full window."
  (interactive)
  (org-agenda-list)
  (delete-other-windows))

;; --------------------------------------------------------------------
;; Delete other windows and split right
;; --------------------------------------------------------------------
(defun delete-other-windows-and-split-right ()
  "Delete all other windows and split right."
  (interactive)
  (delete-other-windows)
  (split-window-right))

;; --------------------------------------------------------------------
;; Window splitter helpers
;; --------------------------------------------------------------------
(defun hydra-move-splitter-left (arg)
  "Move window splitter left.  ARG is the number of steps."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right.  ARG is the number of steps."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up.  ARG is the number of steps."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down.  ARG is the number of steps."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(defun window-split-into-3-columns ()
  "Split the window into three columns."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-horizontally)
  (balance-windows))

(defun window-split-into-2-columns-and-a-row ()
  "Split the window into two columns and split the second column into two rows."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (other-window 1)
  (split-window-below)
  (balance-windows))

(defun window-split-into-4 ()
  "Split the window into two columns and split the second column into two rows."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (split-window-below)
  (other-window 2)
  (split-window-below)
  (balance-windows))

;; --------------------------------------------------------------------
;; Hydra keyboard timeout function
;; --------------------------------------------------------------------
(defun hydra-quit-after (seconds)
  "Hydra keyboard quit after SECONDS."
  (let ((timer (timer-create)))
    (timer-set-time timer (timer-relative-time (current-time) seconds))
    (timer-set-function timer 'hydra-keyboard-quit)
    (timer-activate timer)))

;; --------------------------------------------------------------------
;; Org babel
;; --------------------------------------------------------------------
(defun auto-refresh-inline-images ()
  "Used with GraphViz to autorefresh the inserted image."
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

;; --------------------------------------------------------------------
;; Only fundamental-mode in big files
;; --------------------------------------------------------------------
(defun fundamental-mode-in-big-files ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) (* 1024 10024))
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)))

(add-hook 'find-file-hook 'fundamental-mode-in-big-files)

;; --------------------------------------------------------------------
;; Sacha's defun insert defun
;; --------------------------------------------------------------------
(defun org-insert-defun-code (function)
  "Insert an Org source block with the definition for FUNCTION."
  (interactive (find-function-read))
  (let* ((buffer-point (condition-case nil (find-definition-noselect function nil) (error nil)))
         (new-buf (car buffer-point))
         (new-point (cdr buffer-point))
         definition)
    (if buffer-point
        (with-current-buffer new-buf ;; Try to get original definition
          (save-excursion
            (goto-char new-point)
            (setq definition (buffer-substring-no-properties (point) (save-excursion (end-of-defun) (point))))))
      ;; Fallback: Print function definition
      (setq definition (concat (prin1-to-string (symbol-function function)) "\n")))
    (insert "#+begin_src emacs-lisp\n" definition "#+end_src\n")))

;; --------------------------------------------------------------------
;; Extract from archive
;; --------------------------------------------------------------------
(defun archive-extract-to-file (archive-name item-name command dir)
  "Extract ITEM-NAME from ARCHIVE-NAME using COMMAND. Save to
DIR."
  (unwind-protect
      ;; remove the leading / from the file name to force
      ;; expand-file-name to interpret its path as relative to dir
      (let* ((file-name (if (string-match "\\`/" item-name)
                            (substring item-name 1)
                          item-name))
             (output-file (expand-file-name file-name dir))
             (output-dir (file-name-directory output-file)))
        ;; create the output directory (and its parents) if it does
        ;; not exist yet
        (unless (file-directory-p output-dir)
          (make-directory output-dir t))
        ;; execute COMMAND, redirecting output to output-file
        (apply #'call-process
               (car command)            ;program
               nil                      ;infile
               `(:file ,output-file)    ;destination
               nil                      ;display
               (append (cdr command) (list archive-name item-name))))
    ;; FIXME: add unwind forms
    nil))

(defun archive-extract-marked-to-file (output-dir)
  "Extract marked archive items to OUTPUT-DIR."
  (interactive "sOutput directory: ")
  (let ((command (symbol-value (archive-name "extract")))
        (archive (buffer-file-name))
        (items (archive-get-marked ?* t))) ; get marked items; t means
                                        ; get item under point if
                                        ; nothing is marked
    (mapc
     (lambda (item)
       (archive-extract-to-file archive
                                (aref item 0) ; get the name from the descriptor
                                command output-dir))
     items)))

(provide 'custom-functions)
;;; custom-functions ends here
