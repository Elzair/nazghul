(load "naz.scm")
(kern-load "game.scm")
(load "tests/test-map-1.scm")

(define hour 12)
(define minutes 45)
(define time-in-minutes (+ (* hour 60) minutes))

(kern-set-clock 
 0 ; year
 0 ; month
 0 ; week
 0 ; day
 hour  ; hour
 minutes ; minutes(
 )


(kern-mk-astral-body
 'sun              ; tag
 "Fyer (the sun)"  ; name
 1                 ; relative astronomical distance 
 1                 ; minutes per phase (n/a for sun)
 (/ (* 24 60) 360) ; minutes per degree
 0                 ; initial arc
 0                 ; initial phase
 '()               ; script interface
 ;; phases:
 (list 
  (list s_sun 255 "full")
  )
 )

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
 (list (list (list 0 -2) (list 1 1)) (list (list 0 -1))   (list (list 0 0)))
 (list (list (list 0 1))   (list (list 0 2))    (list (list 0 1)))
 (list (list (list 0 0))   (list (list 0 -1))   (list (list 0 -2)))
 )

;; These are the tests:
(kern-dtable-push  0  0  0)

(kern-dtable-push  0  1  0)
(kern-dtable-push  0  1  1)

(kern-dtable-push  0  2  0)
(kern-dtable-pop   0  2)

(kern-dtable-push  1  0 -2)
(kern-dtable-push  1  0 -1)
(kern-dtable-pop   1  0)

(kern-dtable-pop   1  1)

(if (= 0 (kern-dtable-get 2 0))
    (kern-dtable-push 2 0 1)
    (kern-dtable-push 2 0 -1))

(kern-dtable-push  2  1  0)
(kern-dtable-push  2  1  1)
(kern-dtable-pop   2  1)

(define (push-a-lot n)
  (if (> n 0)
      (begin
        (kern-dtable-push 2 2 2)
        (push-a-lot (- n 1)))))
(push-a-lot 10)
