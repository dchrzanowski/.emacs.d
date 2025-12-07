;;; package --- Summary
;;; Commentary:
;; --------------------------------------------------------------------
;;; Code:
;; --------------------------------------------------------------------

;; --------------------------------------------------------------------
;; hydra
;; --------------------------------------------------------------------
(use-package hydra)

;; --------------------------------------------------------------------
;; Window operations hydra
;; --------------------------------------------------------------------
(defhydra hydra-window-operations (:color pink :hint nil)
  "
_h_/_j_/_k_/_l_ move     _w_/_r_/_f_/_F_ buf/bkm/proj/file   _x_/_X_/_o_ delete ace/here/other    _n_/_p_ tabs          _D_ired kill all     _B_ imenu
_H_/_J_/_K_/_L_ resize   _b_/_v_/_s2_/_s3_/_s4_ splits         _u_/_U_ undo/redo                  _0_-_9_/_c_ ws/close    _S_plit toggle
_d_ ace            _W_ kill buffer               _i_/_I_ jump other                 _M-u_ clean         _;_ zoxide
_z_ swap           _=_ balance                   _q_uit                           _R_evert buffer     Split mo_V_e
=
"
  ("h" windmove-left)
  ("l" windmove-right)
  ("j" windmove-down)
  ("k" windmove-up)
  ("H" (lambda () (interactive) (hydra-move-window-splitter-left 8)))
  ("L" (lambda () (interactive) (hydra-move-window-splitter-right 8)))
  ("J" (lambda () (interactive) (hydra-move-window-splitter-down 4)))
  ("K" (lambda () (interactive) (hydra-move-window-splitter-up 4)))
  ("i" (lambda () (interactive) (other-window 1)) :exit t)
  ("v" dchrzan/split-right-and-follow)
  ("I" aw-flip-window)
  ("n" next-buffer)
  ("p" previous-buffer)
  ("b" split-window-vertically)
  ("V" split-window-horizontally)
  ("o" delete-other-windows)
  ("w" helm-mini)
  ("r" helm-filtered-bookmarks)
  ("R" revert-buffer)
  ("B" helm-semantic-or-imenu)
  ("c" eyebrowse-close-window-config)
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
  ("W" kill-current-buffer)
  ("D" kill-all-dired-buffers)
  (";" zoxide-travel)
  ("F" helm-find-files)
  ("f" helm-projectile-find-file)
  ("M-f" helm-swoop)
  ("s2" window-split-into-2-columns-and-a-row)
  ("s3" window-split-into-3-columns)
  ("s4" window-split-into-4)
  ("d" ace-window :exit t)
  ("u" winner-undo)
  ("U" winner-redo)
  ("M-u" clean-buffer-list)
  ("S" window-toggle-split-direction)
  ("0" eyebrowse-switch-to-window-config-0)
  ("1" eyebrowse-switch-to-window-config-1)
  ("2" eyebrowse-switch-to-window-config-2)
  ("3" eyebrowse-switch-to-window-config-3)
  ("4" eyebrowse-switch-to-window-config-4)
  ("5" eyebrowse-switch-to-window-config-5)
  ("6" eyebrowse-switch-to-window-config-6)
  ("7" eyebrowse-switch-to-window-config-7)
  ("8" eyebrowse-switch-to-window-config-8)
  ("9" eyebrowse-switch-to-window-config-9)
  ("RET" nil "Cancel")
  ("SPC" nil "Cancel")
  ("q" nil :color blue))

;; --------------------------------------------------------------------
;; Text size
;; --------------------------------------------------------------------
(defhydra hydra-text-size (:color pink :hint nil)
  "
_=_/_-_ scale text
=
"
  ("=" text-scale-increase)
  ("-" text-scale-decrease)
  ("RET" nil "Cancel")
  ("SPC" nil "Cancel")
  ("q" nil :color blue))

;; --------------------------------------------------------------------
;; Restclient hydra
;; --------------------------------------------------------------------
(defhydra hydra-restclient (:color pink :hint nil)
  "
_j_ next    _k_ previous    _c_ run
_U_/_u_  run/get user token   _A_/_a_ run/get admin token
=
"
  ("j" restclient-jump-next)
  ("k" restclient-jump-prev)
  ("u" get-user-bearer-token)
  ("a" get-admin-bearer-token)
  ("U" run-user-bearer-token)
  ("A" run-admin-bearer-token)
  ("c" restclient-http-send-current-stay-in-window)
  ("RET" nil "Cancel")
  ("SPC" nil "Cancel")
  ("q" nil :color blue))

;; --------------------------------------------------------------------
;; Expand region hydra
;; --------------------------------------------------------------------
(defhydra hydra-expand-region ()
  "region: "
  ("SPC" nil "Cancel")
  ("k" er/expand-region "expand")
  ("j" er/contract-region "contract"))

;; --------------------------------------------------------------------
;; Auto highlight hydra
;; --------------------------------------------------------------------
(defhydra hydra-todo (:color pink :hint nil)
  "
_j_/_k_ next/prev    _o_ occur
=
"
  ("j" hl-todo-next)
  ("k" hl-todo-previous)
  ("o" hl-todo-occur :exit t)
  ("RET" nil "Cancel")
  ("SPC" nil "Cancel")
  ("q" nil :color blue))

;; --------------------------------------------------------------------
;; Git-gutter hydra
;; --------------------------------------------------------------------
(defhydra hydra-git-gutter (:color pink
                                   :pre (git-gutter-mode 1)
                                   :hint nil)
  "
Git gutter:
  _j_: next hunk        _s_tage hunk     _Q_uit
  _k_: previous hunk    _r_evert hunk    _q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _gg_: first hunk
  _G_: last hunk        set start _R_evision
=
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
  ("RET" nil "Cancel")
  ("SPC" nil "Cancel")
  ("q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear)) :color blue))

;; --------------------------------------------------------------------
;; Flycheck hydra
;; --------------------------------------------------------------------
(defhydra hydra-flycheck (:color pink :hint nil)
  "
_j_/_k_ next/prev    _gg_/_G_ first/last    _f_ set filter    _q_uit    _J_ tide Fix
=
"
  ("f"  flycheck-error-list-set-filter                            "Filter")
  ("j"  flycheck-next-error                                       "Next")
  ("k"  flycheck-previous-error                                   "Previous")
  ("gg" flycheck-first-error                                      "First")
  ("J"  tide-fix                                                  "Tide Fix")
  ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("RET" nil "Cancel")
  ("SPC" nil "Cancel")
  ("q"  nil))

;; --------------------------------------------------------------------
;; Flymake hydra
;; --------------------------------------------------------------------
(defhydra hydra-flymake (:color pink :hint nil)
  "
_j_/_k_ next/prev   _l_ error list   _q_uit
=
"
  ("j"  flymake-goto-next-error         "Next")
  ("k"  flymake-goto-previous-error     "Previous")
  ("l"  flymake-show-diagnostics-buffer "List")
  ("RET" nil "Cancel")
  ("SPC" nil "Cancel")
  ("q"  nil))

;; --------------------------------------------------------------------
;; Ediff hydra
;; --------------------------------------------------------------------
(defhydra hydra-ediff (:color pink :hint nil)
  "
^Buffers           Files           VC                     Ediff regions
----------------------------------------------------------------------
_b_uffers           _f_iles (_=_)       _r_evisions              _l_inewise
_B_uffers (3-way)   _F_iles (3-way)                          _w_ordwise
                  _c_urrent file    _q_uit
=
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
  ("RET" nil "Cancel")
  ("SPC" nil "Cancel")
  ("q" nil "Quit" :color blue))

;; --------------------------------------------------------------------
;; Yasnippet hydra
;; --------------------------------------------------------------------
(defhydra hydra-yasnippet (:color pink :hint nil)
  "
 Modes:  Load/Visit:  Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll     _q_uit
=
"
  ("d" yas-load-directory)
  ("e" yas-activate-extra-mode)
  ("i" yas-insert-snippet)
  ("f" yas-visit-snippet-file :color blue)
  ("n" yas-new-snippet)
  ("t" yas-tryout-snippet)
  ("l" yas-describe-tables)
  ("g" yas/global-mode)
  ("m" yas/minor-mode)
  ("a" yas-reload-all)
  ("RET" nil "Cancel")
  ("SPC" nil "Cancel")
  ("q" nil :color blue))

;; --------------------------------------------------------------------
;; Org agenda hydra
;; --------------------------------------------------------------------
(defhydra hydra-org-agenda (:color pink
                                   :pre (setq which-key-inhibit t)
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
  ("RET" nil "Cancel")
  ("SPC" nil "Cancel")
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

;; --------------------------------------------------------------------
;; Smerge hydra
;; --------------------------------------------------------------------
(defhydra hydra-smerge (:color pink
                               :hint nil
                               :pre (smerge-mode 1))
  "
^Move^  ^Keep^   ^Diff^     ^Pair^
------------------------------------------------------
_n_ext  _b_ase   _R_efine   _<_: base-mine
_p_rev  _m_ine   _E_diff    _=_: mine-other
^ ^     _o_ther  _C_ombine  _>_: base-other
^ ^     _a_ll    _r_esolve
_q_uit _RET_: current
=
"
  ("RET" smerge-keep-current)
  ("C" smerge-combine-with-next)
  ("E" smerge-ediff)
  ("R" smerge-refine)
  ("a" smerge-keep-all)
  ("b" smerge-keep-base)
  ("m" smerge-keep-mine)
  ("n" smerge-next)
  ("o" smerge-keep-other)
  ("p" smerge-prev)
  ("r" smerge-resolve)
  ("<" smerge-diff-base-mine)
  ("=" smerge-diff-mine-other)
  (">" smerge-diff-base-other)
  ("SPC" nil "Cancel")
  ("q" nil :color blue))

;; --------------------------------------------------------------------
;; Smartparens hydra
;; --------------------------------------------------------------------
(defhydra smartparens-hydra (:hint nil)
  "
_d_: down           _a_: back-down        _f_: -> sexp    _k_: hyb-kill      _c_-_a_: begin
_e_: up             _u_: back-up          _b_: <- sexp    _K_: kill          _c_-_e_: end
_[_: back-unwrap    _]_: unwrap           _r_: rewrap     _m_: mark            _j_: join
_p_: prev           _n_: next             _c_: copy       _s_: mark-thing      _|_: split
_t_: transpose      _T_: hyb-transpose    _q_: quit
"

  ("d" sp-down-sexp)
  ("e" sp-up-sexp)
  ("u" sp-backward-up-sexp)
  ("a" sp-backward-down-sexp)
  ("f" sp-forward-sexp)
  ("b" sp-backward-sexp)
  ("k" sp-kill-hybrid-sexp)
  ("t" sp-transpose-sexp)
  ("T" sp-transpose-hybrid-sexp)
  ("K" sp-kill-sexp)
  ("[" sp-backward-unwrap-sexp)
  ("]" sp-unwrap-sexp)
  ("r" sp-rewrap-sexp)
  ("p" sp-previous-sexp)
  ("n" sp-next-sexp)
  ("j" sp-join-sexp)
  ("|" sp-split-sexp)
  ("c" sp-copy-sexp)
  ("s" sp-select-next-thing :color blue)
  ("m" sp-mark-sexp :color blue)
  ("SPC" nil "Cancel")
  ("q" nil :color blue))

(provide 'hydra-setup)
;;; hydra-setup.el ends here
