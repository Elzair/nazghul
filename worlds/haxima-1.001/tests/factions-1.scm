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
              faction-men            ; starting alignment
              0 10 2                ; str/int/dex
              0 1                   ; hp mod/mult
              10 5                  ; mp mod/mult
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
              faction-men            ; starting alignment
              0 10 0                ; str/int/dex
              0 0                   ; hp mod/mult
              0 0                   ; mp mod/mult
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
              faction-men            ; starting alignment
              0 10 0                ; str/int/dex
              0 0                   ; hp mod/mult
              0 0                   ; mp mod/mult
              30 0 9 9              ; hp/xp/mp/lvl
              nil          ; conv
              nil            ; sched
              nil                   ; special ai
              (list t_dagger))      ; readied

(load "tests/empty-party.scm")

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

(if (not (= (kern-being-get-current-faction char) faction-player))
    (error "test error 1"))

(if (not (= (kern-being-get-base-faction char) faction-player))
    (error "test error 2"))

(kern-char-charm char faction-monster)

(if (not (= (kern-being-get-current-faction char) faction-monster))
    (error "test error 3"))

(if (not (= (kern-being-get-base-faction char) faction-player))
    (error "test error 4"))

(kern-char-uncharm char)

(if (not (= (kern-being-get-current-faction char) faction-player))
    (error "test error 5"))

(if (not (= (kern-being-get-base-faction char) faction-player))
    (error "test error 6"))

(kern-char-charm char faction-monster)
(kern-char-charm char faction-orks)

(if (not (= (kern-being-get-current-faction char) faction-orks))
    (error "test error 7"))

(if (not (= (kern-being-get-base-faction char) faction-player))
    (error "test error 8"))

(kern-char-uncharm char)

(if (not (= (kern-being-get-current-faction char) faction-orks))
    (error "test error 9"))

(if (not (= (kern-being-get-base-faction char) faction-player))
    (error "test error 10"))
