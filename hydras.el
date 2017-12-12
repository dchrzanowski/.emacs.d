;;; package --- Summary
;;; Commentary:
;; -------------------------------------------------------------------------------------------------------------------------
;;; Code:
;; -------------------------------------------------------------------------------------------------------------------------

;; -------------------------------------------------------------------------------------------------------------------------
;; Window operations hydra
;; -------------------------------------------------------------------------------------------------------------------------
(defhydra hydra-window-operations (:color pink
                                          :hint nil)
  "
_h_/_j_/_k_/_l_ movement    _w_/_r_/_f_ buffers/bookmarks/files    _x_/_X_/_o_ delete ace/here/other    _n_/_p_ tabs
_H_/_J_/_K_/_L_ resize      _b_/_v_/_2_/_3_/_4_ splits                 _u_/_U_ undo/redo

_d_ ace    _C-w_ kill buffer    _i_ jump other    _M-u_ clean    _z_ swap    _=_ balance
"
  ("h" windmove-left)
  ("l" windmove-right)
  ("j" windmove-down)
  ("k" windmove-up)
  ("H" (lambda () (interactive) (hydra-move-splitter-left 8)))
  ("L" (lambda () (interactive) (hydra-move-splitter-right 8)))
  ("J" (lambda () (interactive) (hydra-move-splitter-down 8)))
  ("K" (lambda () (interactive) (hydra-move-splitter-up 8)))
  ("i" (lambda () (interactive) (other-window 1)) :exit t)
  ("n" tabbar-forward)
  ("p" tabbar-backward)
  ("b" split-window-vertically)
  ("v" split-window-horizontally)
  ("o" delete-other-windows)
  ("w" helm-mini)
  ("r" helm-filtered-bookmarks)
  ("X" delete-window)
  ("x" (lambda ()
         (interactive)
         (ace-window 16)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("z" (lambda ()
         (interactive)
         (ace-window 4)
         (add-hook 'ace-window-end-once-hook
                   'hydra-window/body)))
  ("=" balance-windows)
  ("C-w" kill-this-buffer)
  ("f" helm-find-files)
  ("2" window-split-into-2-columns-and-a-row)
  ("3" window-split-into-3-columns)
  ("4" window-split-into-4)
  ("d" ace-window :exit t)
  ("u" winner-undo)
  ("U" winner-redo)
  ("M-u" clean-buffer-list)
  ("q" nil "Quit" :color blue))

;; -------------------------------------------------------------------------------------------------------------------------
;; Rare launcher hydra
;; -------------------------------------------------------------------------------------------------------------------------
(defhydra hydra-rare-launcher (:color pink :hint nil)
  "
_g_ git-gutter    _t_ git-timemachine    _a_ auto-highlight    _T_ todo    _e_ errors
_c_ zenity        _C_ insert hex         _b_ beautify          _p_/_P_ webpaste region/buffer
"
  ("g" hydra-git-gutter/body :exit t)
  ("t" git-timemachine-toggle :exit t)
  ("p" webpaste-paste-region :exit t)
  ("P" webpaste-paste-buffer :exit t)
  ("c" zenity-cp-color-at-point-dwim :exit t)
  ("C" inder-color-hex :exit t)
  ("a" hydra-auto-highlight/body :exit t)
  ("e" hydra-flycheck/body :exit t)
  ("E" hydra-ediff/body :exit t)
  ("T" hydra-todo/body :exit t)
  ("b" hydra-beautify/body :exit t)
  ("q" nil "Quit" :color blue))

;; -------------------------------------------------------------------------------------------------------------------------
;; Auto highlight hydra
;; -------------------------------------------------------------------------------------------------------------------------
(defhydra hydra-auto-highlight (:color pink :hint nil)
  "
_j_/_k_ next/prev    _e_ edit    _t_ toggle
"
  ("j" ahs-forward)
  ("k" ahs-backward)
  ("e" ahs-edit-mode :exit t)
  ("t" auto-highlight-symbol-mode)
  ("q" nil "Quit" :color blue))

;; -------------------------------------------------------------------------------------------------------------------------
;; Auto highlight hydra
;; -------------------------------------------------------------------------------------------------------------------------
(defhydra hydra-todo (:color pink :hint nil)
  "
_j_/_k_ next/prev    _o_ occur
"
  ("j" hl-todo-next)
  ("k" hl-todo-previous)
  ("o" hl-todo-occur :exit t)
  ("q" nil "Quit" :color blue))

;; -------------------------------------------------------------------------------------------------------------------------
;; Web beautify hydra
;; -------------------------------------------------------------------------------------------------------------------------
(defhydra hydra-beautify (:color pink :hint nil)
  "
_j_ JS    _h_ HTML    _c_ CSS    _u_ untabify    _w_ whitespace    _i_ indent
"
  ("j" web-beautify-js :exit t)
  ("h" web-beautify-html :exit t)
  ("c" web-beautify-css :exit t)
  ("w" whitespace-mode)
  ("i" cleanup-buffer :exit t)
  ("u" untabify-buffer :exit t))

;; -------------------------------------------------------------------------------------------------------------------------
;; Git-gutter hydra
;; -------------------------------------------------------------------------------------------------------------------------
(defhydra hydra-git-gutter (:pre (git-gutter-mode 1)
                                 :hint nil)
  "
Git gutter:
  _j_: next hunk        _s_tage hunk     _Q_uit
  _k_: previous hunk    _r_evert hunk    _q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _gg_: first hunk
  _G_: last hunk        set start _R_evision
"
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("gg" (progn (goto-char (point-min))
               (git-gutter:next-hunk 1)))
  ("G" (progn (goto-char (point-max))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("Q" nil :color blue)
  ("q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear)) :color blue))

;; -------------------------------------------------------------------------------------------------------------------------
;; Flycheck hydra
;; -------------------------------------------------------------------------------------------------------------------------
(defhydra hydra-flycheck (:color pink :hint nil)
  "
_j_/_k_ next/prev    _gg_/_G_ first/last    _f_ set filter
"
  ("f"  flycheck-error-list-set-filter                            "Filter")
  ("j"  flycheck-next-error                                       "Next")
  ("k"  flycheck-previous-error                                   "Previous")
  ("gg" flycheck-first-error                                      "First")
  ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q"  nil))

;; -------------------------------------------------------------------------------------------------------------------------
;; Indent hydra
;; -------------------------------------------------------------------------------------------------------------------------
(defhydra hydra-indent (:color pink :hint nil)
  "
_h_ unindent    _l_ indent    _i_ auto indent
"
  ("h" custom-unindent-region)
  ("l" custom-indent-region)
  ("i" cleanup-buffer)
  ("q" nil :color blue))

;; -------------------------------------------------------------------------------------------------------------------------
;; Ediff hydra
;; -------------------------------------------------------------------------------------------------------------------------
(defhydra hydra-ediff (:color blue :hint nil)
  "
^Buffers           Files           VC                     Ediff regions
----------------------------------------------------------------------
_b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
_B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
                  _c_urrent file    _q_uit
"
  ("b" ediff-buffers)
  ("B" ediff-buffers3)
  ("=" ediff-files)
  ("f" ediff-files)
  ("F" ediff-files3)
  ("c" ediff-current-file)
  ("r" ediff-revision)
  ("l" ediff-regions-linewise)
  ("w" ediff-regions-wordwise)
  ("q" nil "Quit" :color blue))

;; -------------------------------------------------------------------------------------------------------------------------
;; Org agenda hydra
;; -------------------------------------------------------------------------------------------------------------------------
(defhydra hydra-org-agenda (:pre (setq which-key-inhibit t)
                                 :post (setq which-key-inhibit nil)
                                 :hint none)
  "
Org agenda (_q_uit)

^Clock^      ^Visit entry^              ^Date^             ^Other^
^-----^----  ^-----------^------------  ^----^-----------  ^-----^---------
_ci_ in      _SPC_ in other window      _ds_ schedule      _gr_ reload
_co_ out     _TAB_ & go to location     _dd_ set deadline  _._  go to today
_cq_ cancel  _RET_ & del other windows  _dt_ timestamp     _gd_ go to date
_cj_ jump    _o_   link                 _+_  do later      ^^
^^           ^^                         _-_  do earlier    ^^
^^           ^^                         ^^                 ^^
^View^          ^Filter^                 ^Headline^         ^Toggle mode^
^----^--------  ^------^---------------  ^--------^-------  ^-----------^----
_vd_ day        _ft_ by tag              _ht_ set status    _tf_ follow
_vw_ week       _fr_ refine by tag       _hk_ kill          _tl_ log
_vt_ fortnight  _fc_ by category         _hr_ refile        _ta_ archive trees
_vm_ month      _fh_ by top headline     _hA_ archive       _tA_ archive files
_vy_ year       _fx_ by regexp           _h:_ set tags      _tr_ clock report
_vn_ next span  _fd_ delete all filters  _hp_ set priority  _td_ diaries
_vp_ prev span  ^^                       ^^                 ^^
_vr_ reset      ^^                       ^^                 ^^
^^              ^^                       ^^                 ^^
"
  ;; Entry
  ("hA" org-agenda-archive-default)
  ("hk" org-agenda-kill)
  ("hp" org-agenda-priority)
  ("hr" org-agenda-refile)
  ("h:" org-agenda-set-tags)
  ("ht" org-agenda-todo)
  ;; Visit entry
  ("o"   link-hint-open-link :exit t)
  ("<tab>" org-agenda-goto :exit t)
  ("TAB" org-agenda-goto :exit t)
  ("SPC" org-agenda-show-and-scroll-up)
  ("RET" org-agenda-switch-to :exit t)
  ;; Date
  ("dt" org-agenda-date-prompt)
  ("dd" org-agenda-deadline)
  ("+" org-agenda-do-date-later)
  ("-" org-agenda-do-date-earlier)
  ("ds" org-agenda-schedule)
  ;; View
  ("vd" org-agenda-day-view)
  ("vw" org-agenda-week-view)
  ("vt" org-agenda-fortnight-view)
  ("vm" org-agenda-month-view)
  ("vy" org-agenda-year-view)
  ("vn" org-agenda-later)
  ("vp" org-agenda-earlier)
  ("vr" org-agenda-reset-view)
  ;; Toggle mode
  ("ta" org-agenda-archives-mode)
  ("tA" (org-agenda-archives-mode 'files))
  ("tr" org-agenda-clockreport-mode)
  ("tf" org-agenda-follow-mode)
  ("tl" org-agenda-log-mode)
  ("td" org-agenda-toggle-diary)
  ;; Filter
  ("fc" org-agenda-filter-by-category)
  ("fx" org-agenda-filter-by-regexp)
  ("ft" org-agenda-filter-by-tag)
  ("fr" org-agenda-filter-by-tag-refine)
  ("fh" org-agenda-filter-by-top-headline)
  ("fd" org-agenda-filter-remove-all)
  ;; Clock
  ("cq" org-agenda-clock-cancel)
  ("cj" org-agenda-clock-goto :exit t)
  ("ci" org-agenda-clock-in :exit t)
  ("co" org-agenda-clock-out)
  ;; Other
  ("q" nil :exit t)
  ("gd" org-agenda-goto-date)
  ("." org-agenda-goto-today)
  ("gr" org-agenda-redo))

;;; hydras.el ends here
(provide 'hydras)
