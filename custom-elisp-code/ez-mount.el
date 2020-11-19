;;; ez-mount.el --- Notifications for Org-mode

;; Copyright (C) 2012-2018  Free Software Foundation, Inc.

;; Author: Peter Münster <pmrb@free.fr>
;; Keywords: notification, todo-list, alarm, reminder, pop-up

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(defun ez-mount--dev-list(&optional mount)
  "Get a list of removeble devices.  MOUNT."
  (let ((devices-list)
        (lsblk-raw-output (shell-command-to-string "lsblk -P -o NAME,MOUNTPOINT,RM | grep 'RM=\\\"1\\\"'"))
        (devpoint))
    (mapcar (lambda (el)
              (setq devpoint (split-string
                              (replace-regexp-in-string "NAME\=" ""
                                                        (replace-regexp-in-string "MOUNTPOINT\=" "" el t)t) " " t))
              (if mount
                  (when (> (length (replace-regexp-in-string "\"" "" (nth 1 devpoint) t))0)
                    (push (replace-regexp-in-string "\"" "" (nth 0 devpoint)) devices-list)))
              (when (string= "" (replace-regexp-in-string "\"" "" (nth 1 devpoint) t))
                (push (replace-regexp-in-string "\"" "" (nth 0 devpoint)) devices-list)))
            (split-string lsblk-raw-output "\n" t))
    (progn (cl-sort devices-list 'string-lessp :key 'downcase))))

(defun ez-mount(device)
  "Mount a removable device.  DEVICE can be passed or chosen interactively."
  (interactive (list (completing-read "Choose device for mount:" (ez-mount--dev-list nil))))
  (let ((device-name (concat "/dev/" (file-name-base device)))
        (mount-dir (substitute-in-file-name (concat "$HOME/Temp/USB/" (file-name-base device)))))
    (if (file-exists-p device-name)
        (progn
          (unless (file-directory-p mount-dir)
            (make-directory mount-dir t))
          (when (file-directory-p mount-dir)
            (sudo-shell-command (concat "mount " device-name " " mount-dir))))
      (message "%s" (concat "Device" device-name "  not found!")))))

(defun ez-umount(device)
  "Unmount a removable device.  DEVICE can be passed or chosen interactively."
  (interactive (list (completing-read "Choose device for umount:" (ez-mount--dev-list t))))
  (let ((device-name (concat "/dev/" (file-name-base device)))
        (mount-dir (substitute-in-file-name (concat "$HOME/Temp/USB/" (file-name-base device)))))
    (if (file-exists-p device-name)
        (progn
          (sudo-shell-command (concat "umount " mount-dir))
          (delete-directory mount-dir))
      (message "%s" (concat "Device " device-name " not found!")))))

(defun sudo-shell-command (command)
  "Run a command as super user in shell.  COMMAND is the full command to execute."
  (shell-command
   (concat "echo " (shell-quote-argument (read-passwd "Password? ")) " | sudo -S " command)
   "*sudo-shell-command*"
   "*sudo-shell-command error*"))

(provide 'ez-mount)
;;; ez-mount.el ends here
