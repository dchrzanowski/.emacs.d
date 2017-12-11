;;; package --- Summary
;;; Commentary:
;; -------------------------------------------------------------------------------------------------------------------------
;;; Code:
;; -------------------------------------------------------------------------------------------------------------------------
(defhydra hydra-window-boss (:color pink
                             :hint nil)
  "
^Windmove^         ^Split^                  ^Other^            ^Tabbar^
^^^^^^^^-----------------------------------------------------------------------
_h_: left          _v_: split vertical      _o_: delete other  _J_: previous
_l_: right         _b_: split horizontal    _q_: quit          _K_: next
_j_: down          _V_: split vert 'n move  _w_: buffers
_k_: up            _B_: split hor 'n move   _r_: bookmarks
"
  ("h" windmove-left)
  ("l" windmove-right)
  ("j" windmove-down)
  ("k" windmove-up)
  ("b" split-window-vertically)
  ("B" (lambda () (interactive)
         (split-window-vertically)
         (other-window 1)))
  ("v" split-window-horizontally)
  ("V" (lambda () (interactive)
         (split-window-horizontally)
         (other-window 1)))
  ("o" delete-other-windows)
  ("J" tabbar-backward)
  ("K" tabbar-forward)
  ("w" helm-buffers-list)
  ("r" helm-filtered-bookmarks)
  ("q" nil "Quit" :color blue))

;;; hydras.el ends here
(provide 'hydras)
