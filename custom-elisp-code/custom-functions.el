;;; package --- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------
;; --------------------------------------------------------------------
;; Cool new line maker  (makes a proper enter and indent)
;; --------------------------------------------------------------------
(defun dchrzan/newline-and-indent-inside-of-bracket ()
  "Add two newlines and put the cursor at the right indentation.
Between them if a newline is attempted when the cursor is between
two curly braces, otherwise do a regular newline and indent"
  (interactive)
  (progn (newline-and-indent)
         (split-line)
         (indent-for-tab-command)))

;; --------------------------------------------------------------------
;; Set tabs mode
;; --------------------------------------------------------------------
(defun dchrzan/tabs-instead-of-spaces ()
  "Set tabs instead of spaces."
  (interactive)
  (setq-default indent-tabs-mode t))

(defun dchrzan/spaces-instead-of-tabs ()
  "Set spaces instead of tabs."
  (interactive)
  (setq-default indent-tabs-mode nil))

;; --------------------------------------------------------------------
;; Beautifier
;; --------------------------------------------------------------------
(defun untabify-buffer ()
  "Remove tabs."
  (interactive)
  (untabify (point-min) (point-max)))

(defun tabify-buffer ()
  "Remove spaces."
  (interactive)
  (tabify (point-min) (point-max)))

(defun indent-buffer ()
  "Fix indentation."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer-untabify ()
  "Perform a bunch of operations on the whitespace content of a buffer and untabify buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-buffer-tabify ()
  "Perform a bunch of operations on the whitespace content of a buffer and tabify buffer."
  (interactive)
  (indent-buffer)
  (tabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region BEG END."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

;; --------------------------------------------------------------------
;; Neo tree helpers
;; --------------------------------------------------------------------
(defun neotree-open-xdg-on-point ()
  "Open a file under point."
  (interactive)
  (call-process "xdg-open" nil 0 nil
                (neo-buffer--get-filename-current-line)))

(defun neotree-enter-and-close-neotree ()
  "Open a file under point and close neotree."
  (interactive)
  (neotree-enter)
  (neotree-hide))

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
  "Always paste in from the zero register."
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
;; insert timestamps
;; --------------------------------------------------------------------
(defun insert-date-time ()
  "Insert date time."
  (interactive)
  (insert (concat
           "["
           (format-time-string "%d %b %Y %H:%M:%S" (current-time))
           "]")))

(defun insert-time ()
  "Insert time."
  (interactive)
  (insert (concat
           "["
           (format-time-string "%H:%M:%S" (current-time))
           "]")))

;; --------------------------------------------------------------------
;; custom keyboard quit to assits with evil as well
;; --------------------------------------------------------------------
(defun keyboard-quit-and-remove-evil-mc ()
  "Remove evil-mc cursors first and then does a standard `keyboard-quit'."
  (interactive)
  (when evil-mc-cursor-state
    (evil-mc-make-and-goto-first-cursor))
  (evil-normal-state)
  (keyboard-quit))

;; --------------------------------------------------------------------
;; custom keyboard quit to assist with company abort
;; --------------------------------------------------------------------
(defun company-abort-and-switch-to-normal-state ()
  "Abort company and switch no normal state."
  (interactive)
  (company-abort)
  (evil-normal-state))

;; --------------------------------------------------------------------
;; Auto correct previous word and move forward a word
;; --------------------------------------------------------------------
(defun auto-correct-and-move-forward ()
  "Auto correct previous word and move forward a one word."
  (interactive)
  (backward-word)
  (flyspell-auto-correct-word)
  (forward-word))

;; --------------------------------------------------------------------
;; Org-agenda full screen
;; --------------------------------------------------------------------
(defun org-agenda-list-and-delete-other-windows ()
  "Display org agenda in full window."
  (interactive)
  (org-agenda-list)
  (delete-other-windows))

(defun org-agenda-todo-items ()
  "Org agenda show todo items."
  (interactive)
  (org-agenda nil "t"))

(defun org-agenda-search-items ()
  "Org agenda search for items."
  (interactive)
  (org-agenda nil "s"))

(defun org-agenda-match-tag-items ()
  "Org agenda search for tags."
  (interactive)
  (org-agenda nil "m"))

;; --------------------------------------------------------------------
;; Delete other windows and split right
;; --------------------------------------------------------------------
(defun dchrzan/delete-other-windows-and-split-right ()
  "Delete all other windows and split right."
  (interactive)
  (delete-other-windows)
  (split-window-right)
  (windmove-right))

;; --------------------------------------------------------------------
;; Split right and move right
;; --------------------------------------------------------------------
(defun dchrzan/split-right-and-follow ()
  "Delete all other windows and split right."
  (interactive)
  (split-window-right)
  (windmove-right))

;; --------------------------------------------------------------------
;; Window splitter helpers
;; --------------------------------------------------------------------
(defun hydra-move-window-splitter-left (arg)
  "Move window splitter left.  ARG is the number of steps."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-window-splitter-right (arg)
  "Move window splitter right.  ARG is the number of steps."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-window-splitter-up (arg)
  "Move window splitter up.  ARG is the number of steps."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-window-splitter-down (arg)
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
;; (defun fundamental-mode-in-big-files ()
;;   "If a file is over a given size, make the buffer read only."
;;   (when (> (buffer-size) (* 1024 50024))
;;     (setq buffer-read-only t)
;;     (buffer-disable-undo)
;;     (fundamental-mode)))

;; (add-hook 'find-file-hook 'fundamental-mode-in-big-files)

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
  "Extract ITEM-NAME from ARCHIVE-NAME using COMMAND.  Save to DIR."
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


;; --------------------------------------------------------------------
;; Launch dired from file under point
;; --------------------------------------------------------------------
(defun helm-ff-open-dired-at-point ()
  "Launch Dired from file unde point."
  (interactive)
  (helm-select-nth-action 4))

;; --------------------------------------------------------------------
;; Kill all dired buffers
;; --------------------------------------------------------------------
(defun kill-all-dired-buffers ()
  "Kill all Dired buffers."
  (interactive)
  (mapc (lambda (buffer)
          (when (eq 'dired-mode (buffer-local-value 'major-mode buffer))
            (kill-buffer buffer)))
        (buffer-list)))

;; (add-hook 'kill-emacs-hook #'kill-dired-buffers)

;; --------------------------------------------------------------------
;; Dired sort by name, size, date...
;; --------------------------------------------------------------------
(defun xah-dired-sort ()
  "Sort Dired dir listing in different ways.
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
     (t (error "Logic error 09535" )))
    (dired-sort-other $arg )))

;; --------------------------------------------------------------------
;; Dired start process async with marked files
;; --------------------------------------------------------------------
(defvar dired-filelist-cmd
  '(("vlc" "-L")))

(defun dired-start-process (cmd &optional file-list)
  (interactive
   (let ((files (dired-get-marked-files t current-prefix-arg)))
     (list
      (dired-read-shell-command "& on %s: " current-prefix-arg files)
      files)))
  (let ((execution-flag (cadr (assoc cmd dired-filelist-cmd))))
    (apply
     #'call-process
     (list shell-file-name nil 0 nil shell-command-switch
           (format "nohup 1>/dev/null 2>/dev/null %s \"%s\""
                   (if (and (> (length file-list) 1) execution-flag)
                       (format "%s %s" cmd execution-flag)
                     cmd)
                   (mapconcat #'expand-file-name file-list "\" \""))))))

;; --------------------------------------------------------------------
;; Dired async completed message
;; --------------------------------------------------------------------
(defun dired-async-message-function-notify-os (text face &rest args)
  "Notify end of operation in through OS notify."
  (message nil)
  (let* ((msg-string (if args (apply #'format text args) text)))
    (notifications-notify
     :title "Emacs file operation completed"
     :body msg-string)))

;; --------------------------------------------------------------------
;; Calc
;; --------------------------------------------------------------------
(defun calc-eval-string (x)
  "Evaluate a raw string X for calc."
  (interactive)
  (calc-eval x))

(defun calc-eval-region (arg)
  "Evaluate an expression in calc and communicate the result.

If the region is active evaluate that, otherwise search backwards
to the first whitespace character to find the beginning of the
expression.  By default, replace the expression with its value.  If
called with the universal prefix argument, keep the expression
and insert the result into the buffer after it.  If called with a
negative prefix argument, just echo the result in the
minibuffer."
  (interactive "p")
  (let (start end)
    (if (use-region-p)
        (setq start (region-beginning) end (region-end))
      (setq end (point))
      (setq start (search-backward-regexp "\\s-\\|\n" 0 1))
      (setq start (1+ (if start start 0)))
      (goto-char end))
    (let ((value (calc-eval (buffer-substring-no-properties start end))))
      (pcase arg
        (1 (delete-region start end))
        (4 (insert " = ")))
      (pcase arg
        ((or 1 4) (insert value))
        (-1 (message value))))))

(defun calc-eval-line ()
  "Calculate math expression on current line using `calc-eval'."
  (interactive)
  (setq cLine
        (buffer-substring-no-properties
         (line-beginning-position)
         (line-end-position)))
  (save-excursion
    (end-of-line)
    (open-line 1))
  (forward-line 1)
  (insert (calc-eval '("evalv($)" calc-internal-prec 18) 'num cLine)))

;; --------------------------------------------------------------------
;; Frame
;; --------------------------------------------------------------------
(defun dchrzan/new-frame ()
  "Create a new frame with a preset 'default-frame-alist' size."
  (interactive)
  (add-to-list 'default-frame-alist '(height . 60))
  (add-to-list 'default-frame-alist '(width . 160))
  (make-frame))

;; --------------------------------------------------------------------
;; Byte recompile config files
;; --------------------------------------------------------------------
(defun byte-recompile-config-files ()
  "Recompile all Emacs configuration files."
  (interactive)
  (byte-recompile-file "~/.emacs.d/custom-elisp-code/custom-functions.el" 1 0)
  (byte-recompile-file "~/.emacs.d/custom-elisp-code/helm-fzf.el" 1 0)
  (byte-recompile-file "~/.emacs.d/custom-elisp-code/magit-pretty-graph.el" 1 0)
  (byte-recompile-file "~/.emacs.d/custom-elisp-code/org-menu.el" 1 0)
  (byte-recompile-file "~/.emacs.d/custom-elisp-code/org-notify.el" 1 0)
  (byte-recompile-file "~/.emacs.d/custom-elisp-code/saved-macros.el" 1 0)
  (byte-recompile-directory "~/.emacs.d/custom-elisp-code/bookmark-plus" 0 1)
  (byte-recompile-directory "~/.emacs.d/setup-files" 0 1))

;; --------------------------------------------------------------------
;; Toggle between a vertical and horizontal window split
;; --------------------------------------------------------------------
(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

;; --------------------------------------------------------------------
;; Toggle between line number modes
;; --------------------------------------------------------------------
(defun dchrzan/toggle-line-number ()
  "Toggle between line number modes."
  (interactive)
  (if display-line-numbers
      (setq display-line-numbers 'nil)
    (setq display-line-numbers 'relative)))

;; --------------------------------------------------------------------
;; Package info in Emacs
;; --------------------------------------------------------------------
(defun std::pacman-pkg-info ()
  (interactive)
  (let* ((completions (->> "pacman -Q"
                           (shell-command-to-string)
                           (s-trim)
                           (s-lines)
                           (--map (car (s-split " " it :no-nulls)))))
         (name (completing-read "Package: " completions)))
    (switch-to-buffer (get-buffer-create "*Package Info*"))
    (erase-buffer)
    (-> (format "pacman -Qi %s" name)
        (shell-command-to-string)
        (s-trim)
        (insert))
    (goto-char 0)
    (conf-mode)))

;; --------------------------------------------------------------------
;; dakra's ipinfo
;; --------------------------------------------------------------------
(defun ipinfo (ip)
  "Return ip info from ipinfo.io for IP."
  (interactive "sEnter IP to query (blank for own IP): ")
  (require 'request)
  (request
    (concat "https://ipinfo.io/" ip)
    :headers '(("User-Agent" . "Emacs ipinfo.io Client")
               ("Accept" . "application/json")
               ("Content-Type" . "application/json;charset=utf-8"))
    :parser 'json-read
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (message
                 (mapconcat
                  (lambda (e)
                    (format "%10s: %s" (capitalize (symbol-name (car e))) (cdr e)))
                  data "\n"))))
    :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                          (message "Can't receive ipinfo. Error %S " error-thrown)))))

;; ---------------------------------------------------------------------
;; Sum up all TIME-DAYS properties and put them in the current heading's
;; TIME-DAYS-SUM property.
;; ---------------------------------------------------------------------
(defun org-sum-time-days-in-file ()
  "Add up all the TIME-DAYS properties of headings underneath the current one.
The total is written to the TIME-DAYS-SUM property of this heading."
  (interactive)
  (org-entry-put (point) "TIME-DAYS-SUM"
                 (number-to-string
                  (let ((total 0))
                    (org-map-entries
                     (lambda ()
                       (let ((n (org-entry-get (point) "TIME-DAYS")))
                         (when (stringp n)
                           (setq total (+ total (string-to-number n)))))))
                    total))))

;; --------------------------------------------------------------------
;; insert shell option from man pages
;; --------------------------------------------------------------------
(defun insert-shell-option (cmd)
  (interactive "sCommand: ")
  (let ((options ()))
    (require 'pcmpl-args)
    (require 'helm)
    (dolist (item (pcmpl-args-extract-argspecs-from-manpage cmd))
      (let ((option (plist-get item 'option))
            (help (plist-get item :help)))
        (push (cons (with-temp-buffer
                      (insert help)
                      (let ((fill-column 80))
                        (fill-paragraph))
                      (goto-char (point-min))
                      (insert (format "%s\n" option))
                      (buffer-string))
                    (format "%s" option))
              options)))
    (helm (helm-build-sync-source "Options: "
            :candidates (nreverse options)
            :multiline t
            :action #'insert))))

;; --------------------------------------------------------------------
;; create id's for org headings, modified slightly from the original.
;; Thanks to https://writequit.org/articles/emacs-org-mode-generate-ids.html
;; --------------------------------------------------------------------
(defun dchrzan/org-custom-id-get (&optional pom create prefix)
  "Get the ID property of the entry at point-or-marker POM.

If POM is nil, refer to the entry at point.  If the entry does
not have an ID, the function returns nil.  However, when
CREATE is non nil, create a ID if none is present already.
PREFIX will be passed through to `org-id-new'.  In any
case, the ID of the entry is returned."
  (interactive)
  (org-with-point-at pom
    (let ((id (org-entry-get nil "ID")))
      (cond
       ((and id (stringp id) (string-match "\\S-" id))
        id)
       (create
        (setq id (org-id-new prefix))
        (org-entry-put pom "ID" id)
        (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
        id)))))
(defun dchrzan/org-add-ids-to-headlines-in-file (prefix)
  "Add ID properties to all headlines in the current file.

PREFIX is added in front of each generated id."
  (interactive "sPrefix[Enter for blank]: ")
  (org-map-entries (lambda () (dchrzan/org-custom-id-get (point) 'create prefix))))

;; --------------------------------------------------------------------
;; Utilize explainshell.com to provide an explanation of shell commands
;; --------------------------------------------------------------------
(defun explain-shell (cmd)
  "Explain a shell command CMD."
  (interactive (list (read-shell-command "Command: ")))
  (browse-url (format "http://explainshell.com/explain?cmd=%s"
                      (url-encode-url cmd))))

;; --------------------------------------------------------------------
;; Display the current buffer's file metadata
;; --------------------------------------------------------------------
(defun file-metadata ()
  "Show info about the file associated with the currently opened buffer."
  (interactive)
  (let* ((fname (buffer-file-name))
         (data (file-attributes fname))
         (access (current-time-string (nth 4 data)))
         (mod (current-time-string (nth 5 data)))
         (change (current-time-string (nth 6 data)))
         (size (nth 7 data))
         (mode (nth 8 data)))
    (message
     "%s:
  Accessed: %s
  Modified: %s
  Changed: %s
  Size: %s bytes
  Mode: %s"
     fname access mod change size mode)))

;; --------------------------------------------------------------------
;; Call func when y
;; --------------------------------------------------------------------
(defun dchrzan/call-func-on-y (prompt func)
  "PROMPT user with a y or n question, on y call FUNC."
  (when (y-or-n-p (format "%s " prompt))
    (funcall func)))

;; --------------------------------------------------------------------
;; Switch to Mu4e
;; --------------------------------------------------------------------
(defun dchrzan/switch-to-mu4e ()
  "Delete all other windows and switch to Mu4e."
  (interactive)
  (delete-other-windows)
  (mu4e))

;; --------------------------------------------------------------------
;; Switch to init.el
;; --------------------------------------------------------------------
(defun dchrzan/switch-to-init-el ()
  "Delete all other windows and switch to init.el."
  (interactive)
  (delete-other-windows)
  (find-file "~/.emacs.d/init.el"))

;; --------------------------------------------------------------------
;; Switch to Dired
;; --------------------------------------------------------------------
(defun dchrzan/switch-to-dired-two-panel ()
  "Delete all other windows and switch to Dired two panel setup."
  (interactive)
  (delete-other-windows)
  (dired "~/")
  (split-window-horizontally)
  (other-window 1)
  (dired "~/Downloads"))

;; --------------------------------------------------------------------
;; Switch to calendar.org
;; --------------------------------------------------------------------
(defun dchrzan/switch-to-calendar-org ()
  "Delete all other windows and open calendar.org."
  (interactive)
  (eyebrowse-switch-to-window-config 7)
  (delete-other-windows)
  (find-file "~/GoogleDrive/org/work_calendar.org")
  (split-window-horizontally)
  (other-window 1)
  (find-file "~/GoogleDrive/org/calendar.org")
  (other-window 1))

;; --------------------------------------------------------------------
;; Switch to work_calendar.org
;; --------------------------------------------------------------------
(defun dchrzan/switch-to-work-calendar-org ()
  "Delete all other windows and open work_calendar.org."
  (interactive)
  (eyebrowse-switch-to-window-config 7)
  (delete-other-windows)
  (find-file "~/GoogleDrive/org/work_calendar.org"))

;; --------------------------------------------------------------------
;; Call a function when mode not active
;; --------------------------------------------------------------------
(defun dchrzan/call-func-when-mode-not-in-windows (mode func)
  "Call FUNC when MODE is not active in any currently opened windows."
  (let ((windows (window-list))
        (has-desired-mode nil))
    (dolist (window windows)
      (with-current-buffer (window-buffer window)
        (when (eq major-mode mode)
          (setq has-desired-mode t))))
    (unless has-desired-mode
      (funcall func))))

;; --------------------------------------------------------------------
;; RGB value to percent
;; --------------------------------------------------------------------
(defun dchrzan/rgb-to-percent-scale (rgb-val)
  "Convert a value in between 0 and 255 to a 0.000 - 1.000 scale.

Provide the RGB value as the RGB-VAL parameter or enter the value interactively.
Value is automatically inserted as a side effect."

  (interactive "nRGB Value:")
  (if (or (< rgb-val 0) (> rgb-val 255))
      (message "Input value should be in between 0 and 255")
    (insert (format "%.3f" (/ rgb-val 255.0)))))

;; --------------------------------------------------------------------
;; HEX numbers
;; --------------------------------------------------------------------
(defun dchrzan/hex-string-to-num-string (hex)
  "Convert HEX as string to decimal."
  (calc-eval (format "16#%s" hex)))

(defun dchrzan/hex-color-to-rgb (hex-color)
  "Convert a 6 digit HEX-COLOR e.g. '#FF00FF to a formatted '255, 0, 255' string and insert it into the buffer."
  (interactive "sColor as 6 digit HEX:")
  (let ((red (dchrzan/hex-string-to-num-string
              (substring hex-color 1 3)))
        (green (dchrzan/hex-string-to-num-string
                (substring hex-color 3 5)))
        (blue (dchrzan/hex-string-to-num-string
               (substring hex-color 5 7))))
    ;; format and insert
    (insert (format "%s, %s, %s" red green blue))))

;; --------------------------------------------------------------------
;; Native compile packages
;; --------------------------------------------------------------------
(defun dchrzan/native-compile-packages ()
  "Native compilation of the elpa folder."
  (interactive)
  (native-compile-async (concat user-emacs-directory "elpa/") 'recursively))

;; --------------------------------------------------------------------
;; LSP Helpers
;; --------------------------------------------------------------------
(defun dchrzan/lsp-ui-doc-unfocus-and-hide ()
  "Unfocus the UI Doc window and hide automatically."
  (interactive)
  (lsp-ui-doc-unfocus-frame)
  (lsp-ui-doc-hide))

(defun dchrzan/split-and-navigate-to-symbol ()
  "Deletes all other windows, splits the screen and navigates to symbol."
  (interactive)
  (split-window-right)
  (windmove-right)
  (balance-windows)
  (lsp-ui-peek-find-definitions))

;; --------------------------------------------------------------------
;; hledger Helpers
;; --------------------------------------------------------------------
(defun hledger-popup-balance-at-point ()
  "Show balance for account at point in a popup."
  (interactive)
  ;; (if-let ((account (thing-at-point 'hledger-account)))
  (if-let ((account (symbol-at-point)))
      (message (hledger-shell-command-to-string
                (format " balance -N %s " account)))
    (message "No account at point")))

(defun hledger-next-entry ()
  "Move to next entry and pulse."
  (interactive)
  (hledger-next-or-new-entry)
  (hledger-pulse-momentary-current-entry))

(defun hledger-prev-entry ()
  "Move to last entry and pulse."
  (interactive)
  (hledger-backward-entry)
  (hledger-pulse-momentary-current-entry))

(provide 'custom-functions)
;;; custom-functions.el ends here
