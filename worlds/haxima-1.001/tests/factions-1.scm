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
              nil                   ; readied
              )

(kern-mk-char 'ch_shroom ; tag
              "Shroom"              ; name
              sp_human              ; species
              oc_druid              ; occ
              s_companion_druid     ; sprite
              align-town            ; starting alignment
              0 10 0                ; str/int/dex
              0 0                   ; hp mod/mult
              0 0                   ; mp mod/mult
              0 0                   ; hit mod def mod
              0 0                   ; dam mod arm mod
              30 0 9 9              ; hp/xp/mp/lvl
              nil          ; conv
              nil            ; sched
              nil                   ; special ai
              (list t_dagger))      ; readied

(kern-mk-char 'ch_broom ; tag
              "Broom"              ; name
              sp_human              ; species
              oc_druid              ; occ
              s_companion_druid     ; sprite
              align-town            ; starting alignment
              0 10 0                ; str/int/dex
              0 0                   ; hp mod/mult
              0 0                   ; mp mod/mult
              0 0                   ; hit mod def mod
              0 0                   ; dam mod arm mod
              30 0 9 9              ; hp/xp/mp/lvl
              nil          ; conv
              nil            ; sched
              nil                   ; special ai
              (list t_dagger))      ; readied

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
 nil ;; party members
 )

(kern-party-add-member player ch_olin)
(kern-party-add-member player ch_shroom)
(kern-party-add-member player ch_broom)

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
 (list (list (list 0 -2)) (list (list 0 -1))   (list (list 0 0)))
 (list (list (list 0 1))   (list (list 0 2))    (list (list 0 1)))
 (list (list (list 0 0))   (list (list 0 -1))   (list (list 0 -2)))
 )

;; These are the tests:
(define char ch_olin)
(define char2 ch_shroom)
(define char3 ch_broom)

;; basic push/pop
(kern-being-push-faction char 4)
(kern-being-pop-faction char)

;; remove 
(define h1 (kern-being-push-faction char2 2))
(kern-being-push-faction char2 3)
(kern-being-rm-faction char2 h1)

;; has-faction?
;; should be true:
(if (kern-being-has-faction? char2)
    (kern-being-push-faction char2 5))
;; should be false:
(if (not (kern-being-has-faction? char2))
    (kern-being-push-faction char2 6))

;; bottom-faction?
;; should be true:
(if (kern-being-bottom-faction? char)
    (kern-being-push-faction char 7))
;; should be false:
(if (kern-being-bottom-faction? char)
    (kern-being-push-faction char 8))

;; try to pop past the end
(kern-being-pop-faction char3)
(kern-being-pop-faction char3)

;; try to remove a bad handle
(kern-being-rm-faction char 99)
(kern-being-rm-faction char3 h1)
