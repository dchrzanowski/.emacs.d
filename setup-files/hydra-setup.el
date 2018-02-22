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
_h_/_j_/_k_/_l_ movement    _w_/_r_/_f_/_F_ buffers/bookmarks/files    _x_/_X_/_o_ delete ace/here/other    _n_/_p_ tabs
_H_/_J_/_K_/_L_ resize      _b_/_v_/_s2_/_s3_/_s4_ splits                _u_/_U_ undo/redo                  _0_-_9_ worspaces
_d_ ace    _C-w_ kill buffer    _i_/_I_ jump other    _M-u_ clean    _z_ swap    _=_ balance    _q_uit
============================================================================================================================
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
  ("I" aw-flip-window :exit t)
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
  ("F" helm-projectile-find-file)
  ("s2" window-split-into-2-columns-and-a-row)
  ("s3" window-split-into-3-columns)
  ("s4" window-split-into-4)
  ("d" ace-window :exit t)
  ("u" winner-undo)
  ("U" winner-redo)
  ("M-u" clean-buffer-list)
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
  ("q" nil :color blue))

;; --------------------------------------------------------------------
;; Rare launcher hydra
;; --------------------------------------------------------------------
(defhydra hydra-rare-launcher (:color pink :hint nil)
  "
_g_it-gutter    gi_t_-timemachine    _a_uto-highlight    _T_odo      _D_arkroom    _r_ainbow mode    web_p_/_P_aste region/buffer
_z_enity        insert he_x_         _b_eautify          _e_rrors    p_o_midor     e_d_iff
============================================================================================================================
"
  ("g" hydra-git-gutter/body :exit t)
  ("t" git-timemachine-toggle :exit t)
  ("p" webpaste-paste-region :exit t)
  ("P" webpaste-paste-buffer :exit t)
  ("z" zenity-cp-color-at-point-dwim :exit t)
  ("x" insert-color-hex :exit t)
  ("a" hydra-auto-highlight/body :exit t)
  ("e" hydra-flycheck/body :exit t)
  ("E" hydra-ediff/body :exit t)
  ("T" hydra-todo/body :exit t)
  ("b" hydra-beautify/body :exit t)
  ("o" pomidor :exit t)
  ("d" hydra-ediff/body :exit t)
  ("r" rainbow-mode :exit t)
  ("D" darkroom-mode :exit t)
  ("q" nil :color blue))

;; --------------------------------------------------------------------
;; Auto highlight hydra
;; --------------------------------------------------------------------
(defhydra hydra-auto-highlight (:color pink :hint nil)
  "
_j_/_k_ next/prev    _e_ edit    _t_ toggle
"
  ("j" ahs-forward)
  ("k" ahs-backward)
  ("e" ahs-edit-mode :exit t)
  ("t" auto-highlight-symbol-mode)
  ("q" nil :color blue))

;; --------------------------------------------------------------------
;; Auto highlight hydra
;; --------------------------------------------------------------------
(defhydra hydra-todo (:color pink :hint nil)
  "
_j_/_k_ next/prev    _o_ occur
"
  ("j" hl-todo-next)
  ("k" hl-todo-previous)
  ("o" hl-todo-occur :exit t)
  ("q" nil :color blue))

;; --------------------------------------------------------------------
;; Web beautify hydra
;; --------------------------------------------------------------------
(defhydra hydra-beautify (:color pink :hint nil)
  "
_j_ JS    _h_ HTML    _c_ CSS    _u_ untabify    _w_ whitespace    _i_ indent
"
  ("j" web-beautify-js :exit t)
  ("h" web-beautify-html :exit t)
  ("c" web-beautify-css :exit t)
  ("w" whitespace-mode)
  ("i" cleanup-buffer :exit t)
  ("u" untabify-buffer :exit t)
  ("q" nil :color blue))

;; --------------------------------------------------------------------
;; Git-gutter hydra
;; --------------------------------------------------------------------
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

;; --------------------------------------------------------------------
;; Flycheck hydra
;; --------------------------------------------------------------------
(defhydra hydra-flycheck (:color pink :hint nil)
  "
_j_/_k_ next/prev    _gg_/_G_ first/last    _f_ set filter    _q_uit
"
  ("f"  flycheck-error-list-set-filter                            "Filter")
  ("j"  flycheck-next-error                                       "Next")
  ("k"  flycheck-previous-error                                   "Previous")
  ("gg" flycheck-first-error                                      "First")
  ("G"  (progn (goto-char (point-max)) (flycheck-previous-error)) "Last")
  ("q"  nil))

;; --------------------------------------------------------------------
;; Indent hydra
;; --------------------------------------------------------------------
(defhydra hydra-indent (:color pink :hint nil)
  "
_h_ unindent    _l_ indent    _i_ auto indent    _q_uit
"
  ("h" custom-unindent-region)
  ("l" custom-indent-region)
  ("i" cleanup-buffer)
  ("q" nil :color blue))

;; --------------------------------------------------------------------
;; Ediff hydra
;; --------------------------------------------------------------------
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

;; --------------------------------------------------------------------
;; Yasnippet hydra
;; --------------------------------------------------------------------
(defhydra hydra-yasnippet (:color blue :hint nil)
  "
 Modes:  Load/Visit:  Actions:

 _g_lobal  _d_irectory    _i_nsert
 _m_inor   _f_ile         _t_ryout
 _e_xtra   _l_ist         _n_ew
         _a_ll     _q_uit
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
  ("q" nil :color blue))

;; --------------------------------------------------------------------
;; Org agenda hydra
;; --------------------------------------------------------------------
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

;; --------------------------------------------------------------------
;; Org-templates hydra
;; --------------------------------------------------------------------
(defhydra hydra-org-template (:color blue :hint nil)
  "
 _c_enter  _q_uote     _e_macs-lisp    _L_aTeX:
 _l_atex   _E_xample   _p_erl          _i_ndex:
 _a_scii   _v_erse     _P_erl tangled  _I_NCLUDE:
 _s_rc     _n_ote      plant_u_ml      _H_TML:
 _h_tml    ^ ^         ^ ^             _A_SCII:
"
  ("s" (hot-expand "<s"))
  ("E" (hot-expand "<e"))
  ("q" (hot-expand "<q"))
  ("v" (hot-expand "<v"))
  ("n" (hot-expand "<not"))
  ("c" (hot-expand "<c"))
  ("l" (hot-expand "<l"))
  ("h" (hot-expand "<h"))
  ("a" (hot-expand "<a"))
  ("L" (hot-expand "<L"))
  ("i" (hot-expand "<i"))
  ("e" (hot-expand "<s" "emacs-lisp"))
  ("p" (hot-expand "<s" "perl"))
  ("u" (hot-expand "<s" "plantuml :file CHANGE.png"))
  ("P" (hot-expand "<s" "perl" ":results output :exports both :shebang \"#!/usr/bin/env perl\"\n"))
  ("I" (hot-expand "<I"))
  ("H" (hot-expand "<H"))
  ("A" (hot-expand "<A"))
  ("<" self-insert-command "ins")
  ("o" nil "quit"))

(defun hot-expand (str &optional mod header)
  "Expand org template.

STR is a structure template string recognised by org like <s.  MOD is a
string with additional parameters to add the begin line of the
structure element.  HEADER string includes more parameters that are
prepended to the element after the #+HEADER: tag."
  (let (text)
    (when (region-active-p)
      (setq text (buffer-substring (region-beginning) (region-end)))
      (delete-region (region-beginning) (region-end))
      (deactivate-mark))
    (when header (insert "#+HEADER: " header) (forward-line))
    (insert str)
    (org-try-structure-completion)
    (when mod (insert mod) (forward-line))
    (when text (insert text))))

(eval-after-load "org"
  '(cl-pushnew
    '("not" "#+BEGIN_NOTES\n?\n#+END_NOTES")
    org-structure-template-alist))

;; --------------------------------------------------------------------
;; Smerge hydra
;; --------------------------------------------------------------------
(defhydra hydra-smerge
  (:color red :hint nil
          :pre (smerge-mode 1))
  "
^Move^ ^Keep^ ^Diff^ ^Pair^
------------------------------------------------------
_n_ext _b_ase _R_efine _<_: base-mine
_p_rev _m_ine _E_diff _=_: mine-other
^ ^ _o_ther _C_ombine _>_: base-other
^ ^ _a_ll _r_esolve
_q_uit _RET_: current
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
  ("q" nil :color blue))

(provide 'hydra-setup)
;;; hydra-setup.el ends here