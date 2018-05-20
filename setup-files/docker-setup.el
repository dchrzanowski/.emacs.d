;;; package -- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; Docker mode
;; --------------------------------------------------------------------
(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode)
  :defer 2)

(use-package docker
  :config
  (docker-global-mode)
  (require 'docker-images)
  (require 'docker-containers)
  (require 'docker-machine)
  (require 'docker-networks)
  (require 'docker-process)
  (require 'docker-utils)
  (require 'docker-volumes))

(use-package yaml-mode
  :mode ("\\.yml\\'" . yaml-mode))

(provide 'docker-setup)
;;; docker-setup ends here
