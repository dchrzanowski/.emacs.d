;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; alerts package
;; --------------------------------------------------------------------
(use-package alert
  :config

  ;; Reminder to stay healthy!
  (defvar stay-healthy--start nil)
  (defvar stay-healthy--timer nil)
  (defun stay-healthy--alert (start-time)
    "Alert the user to stay healthy"
    (let* ((now (time-to-seconds (current-time)))
           (seconds (- now start-time))
           (hours (format-seconds "%h" seconds)))
      (unless (string= hours "0")
        (alert
         ;; (format "You've been working for %s hour(s)!" hours)
         "Drink some water and get up for a minute :)"
         :title "Stay Healthy!"
         ))))

  (defun stay-healthy-start-timer ()
    "Start a timer to keep you healthy"
    (interactive)
    (unless stay-healthy--start
      (setq stay-healthy--start (time-to-seconds (current-time))))
    (setq stay-healthy--timer (run-with-timer 0 (* 60 60) #'stay-healthy--alert stay-healthy--start)))

  (defun stay-healthy-stop-timer ()
    "End the healthy timer"
    (interactive)
    (when stay-healthy--timer
      (cancel-timer stay-healthy--timer)))

  (stay-healthy-start-timer))

;; --------------------------------------------------------------------
;; type-break mode
;; --------------------------------------------------------------------
;; (use-package type-break
;;   :config
;;   (type-break-mode))

(provide 'alerts-setup)
;;; alerts-setup ends here
