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
