(load "naz.scm")
(kern-load "game.scm")
(load "tests/basic-time.scm")
(load "tests/test-map-1.scm")

(kern-mk-char 'ch_olin ; tag
              "Olin the Ghast"      ; name
              sp_ghast              ; species
              nil                   ; occ
              s_ghost               ; sprite
              align-town            ; starting alignment
              0 10 2                ; str/int/dex
              0 1                   ; hp mod/mult
              10 5                  ; mp mod/mult
              0 0                   ; hit mod def mod
              0 0                   ; dam mod arm mod
              240 0 8 8             ; hp/xp/mp/lvl
              nil                   ; conv
              nil                   ; sched
              nil                   ; special ai
              nil)                  ; readied

(kern-mk-player
 'player                     ; tag
 s_companion_fighter         ; sprite
 "Walk"                      ; movement description
 sound-walking               ; movement sound
 1000                        ; food
 500                         ; gold
 (+ align-player align-town) ; alignment
 nil                         ; formation
 nil                         ; campsite map
 nil                         ; campsite formation
 nil                         ; vehicle
 ;; inventory
 (kern-mk-container
  nil ;; type
  nil ;; trap
  nil ;; contents:
  )
 ;; party members
 (list ch_olin)
 )

(kern-mk-place 
 'p_test
 "Test Place"
 nil          ; sprite
 m_test_1
 #f  ;; wraps
 #f  ;; underground
 #t  ;; wilderness
 #f  ;; tmp combat place
 nil ;; subplaces
 nil ;; neighbors
 
 ;; objects:
 (list
  (list player 9 9)
  )
 nil ;; hooks
)


;; ----------------------------------------------------------------------------
;; The diplomacy table. Each entry defines the attitude of the row to the
;; column. Note that attitudes are not necessarily symmetric. Negative values
;; are hostile, positive are friendly.
;; ----------------------------------------------------------------------------
(kern-mk-dtable
 ;;    none play men orks accu
 (list -2   -1    0)
 (list  1    2    1)
 (list  0   -1   -2)
 )

;; These are the tests:
(kern-dtable-set -1  0  0) ;; f1 bad
(kern-dtable-set  0 -1  0) ;; f2 bad
(kern-dtable-set  0  0 -3) ;; lower bound clipped
(kern-dtable-set  0  1  3) ;; upper bound clipped
(kern-dtable-set  0  2  2) ;; ok
(kern-dtable-get -1  0)    ;; f1 bad -- no crash
(kern-dtable-get  0 -1)    ;; f2 bad -- no crash
(kern-dtable-get  0  0)    ;; ok
(kern-dtable-change 1 0  1) ;; ok positive change
(kern-dtable-change 1 1 -1) ;; ok negative change
