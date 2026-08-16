;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; maven helper
;; --------------------------------------------------------------------
(use-package mvn
  :config
  ;; overwrite the mvn function to always run compile in comint-mode
  (with-eval-after-load 'mvn
    (defun mvn (&optional task args)
      "Run mvn `task` in project root directory."
      (interactive)
      (let ((default-directory (mvn-find-root mvn-build-file-name)))
        (if default-directory
            (let ((task (or task (mvn-get-task default-directory))))
              (setq mvn-last-task task)
              (compile (concat mvn-command " " task " " args) t))
          (message "Couldn't find a maven project.")))))

    ;; wrapper around mvn spring run
    (defun mvn-spring-run ()
      (interactive)
      (mvn "spring-boot:run"))
    )


  (provide 'java-setup)
;;; java-setup.el ends here
