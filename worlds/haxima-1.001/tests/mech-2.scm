(load "naz.scm")
(kern-load "game.scm")
(load "tests/basic-night-time.scm")
(load "tests/test-map-1.scm")

(kern-mk-place 
 'p_test
 "Test Place"
 nil          ; sprite
 m_test_1
 #f  ;; wraps
 #f  ;; underground
 #f  ;; wilderness
 #f  ;; tmp combat place
 nil ;; subplaces
 nil ;; neighbors
 
 ;; objects:
 (list
  (list
   (kern-mk-char
    'ch_thorald_greybeard
    "Thorald Greybeard"
    sp_human
    oc_wizard
    s_companion_wizard
    3
    20 30 22
    0 1
    10 5
    0 0
    0 0
    39 0
    240 8
    nil
    nil
    nil
    (list
     t_rpg
     ))
   9 9)
  (list (kern-tag 'portcullis-1 (mk-portcullis)) 5 12)
  (list (mk-lever 'portcullis-1) 6 11)
  (list (mk-stone-lantern) 12 9)
   )
 nil ;; hooks
)

(load "tests/empty-party.scm")
(kern-party-add-member player ch_thorald_greybeard)

;; ----------------------------------------------------------------------------
;; The diplomacy table. Each entry defines the attitude of the row to the
;; column. Note that attitudes are not necessarily symmetric. Negative values
;; are hostile, positive are friendly.
;; ----------------------------------------------------------------------------
(kern-mk-dtable
 ;;    none play men orks accu
 (dtable-row  0    0    0   0   -1   ) ;; none
 (dtable-row  0    2    1   0   -1   ) ;; play
 (dtable-row -1    1    2  -1   -2   ) ;; men
 (dtable-row -1    0   -1   2   -1   ) ;; orks
 (dtable-row -1   -1   -1  -1    2   ) ;; accu
 )
