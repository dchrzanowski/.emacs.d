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
;; Palette personal function
;; -------------------------------------------------------------------------------------------------------------------------
(defun palette-launch-from-kill-ring ()
  "Launch the palette from the content of the kill ring."
  (interactive)
  (palette (concat "#" (substring-no-properties (car kill-ring)))))


(defun palette-paste-in-current-color ()
  "Paste the currently selected color in the palette to the buffer."
  (interactive)
  (insert (palette-current-color)))

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
;; Duplicate line/region
;; -------------------------------------------------------------------------------------------------------------------------
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated.  However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))

;; -------------------------------------------------------------------------------------------------------------------------
;; Delete line from any position
;; -------------------------------------------------------------------------------------------------------------------------
(defun delete-line-from-any-position ()
  "Delete the current line."
  (interactive)
  (beginning-of-line)
  (kill-line)
  (kill-line))

;; -------------------------------------------------------------------------------------------------------------------------
;; Copy word/paste word
;; -------------------------------------------------------------------------------------------------------------------------
(defun get-point (symbol &optional arg)
 "Get the point SYMBOL ARG."
 (funcall symbol arg)
 (point)
)

(defun copy-thing (begin-of-thing end-of-thing &optional arg)
  "Copy thing between BEGIN-OF-THING & END-OF-THING into kill ring ARG."
   (save-excursion
     (let ((beg (get-point begin-of-thing 1))
    	 (end (get-point end-of-thing arg)))
       (copy-region-as-kill beg end)))
)

(defun paste-to-mark(&optional arg)
  "Paste things to mark, or to the prompt in shell-mode"
  (let ((pasteMe
    (lambda()
       (if (string= "shell-mode" major-mode)
         (progn (comint-next-prompt 25535) (yank))
       (progn (goto-char (mark)) (yank) )))))
    (if arg
        (if (= arg 1)
    	nil
          (funcall pasteMe))
      (funcall pasteMe))
  ))

(defun copy-word (&optional arg)
 "Copy words at point into 'kill-ring' ARG."
  (interactive "P")
  (copy-thing 'backward-word 'forward-word arg)
  ;;(paste-to-mark arg)
)

(defun copy-line (&optional arg)
 "Save current line into Kill-Ring without mark the line ARG."
  (interactive "P")
  (copy-thing 'beginning-of-line 'end-of-line arg)
  (paste-to-mark arg)
)

;; -------------------------------------------------------------------------------------------------------------------------
;; End of line and indent
;; -------------------------------------------------------------------------------------------------------------------------
(defun end-of-line-and-indented-new-line ()
  "Jump to end of line, create a newline and indent."
  (interactive)
  (end-of-line)
(newline-and-indent))

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
;; delete other windows and split right
;; ------------------------------------------------------------------------------------------------------------------------
(defun delete-other-windows-and-split-right ()
  "Delete all other windows and split right."
  (interactive)
  (delete-other-windows)
  (split-window-right))

;; -------------------------------------------------------------------------------------------------------------------------
;; helm launch dired from under point
;; -------------------------------------------------------------------------------------------------------------------------
(defun helm-ff-open-dired-at-point ()
  "Open dired from helm find file panel."
  (interactive)
  (helm-select-nth-action 4))

;;; my-functions.el ends here
(provide 'my-functions)
