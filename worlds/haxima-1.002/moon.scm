
;; Given a kernel moon object, find the gate associated with the current
;; phase. This is for the benefit of moongates trying to find a destination
;; gate.
(define (moon-get-current-gate kmoon)
  (let ((gates (gob-data (kern-astral-body-get-gob kmoon)))
        (phase (kern-astral-body-get-phase kmoon)))
    (safe-eval (list-ref gates phase))))

(define (moon-signal-gate moon phase signal)
  (let ((kgate (safe-eval (list-ref moon phase))))
    (if (not (null? kgate))
        (signal-kobj kgate signal kgate))))

(define (moon-phase-change kmoon old-phase new-phase)
  (let ((moon (gob-data (kern-astral-body-get-gob kmoon))))
    (moon-signal-gate moon old-phase 'off)
    (moon-signal-gate moon new-phase 'on)))

(define source-moon-ifc
  (ifc '()
       (method 'phase-change moon-phase-change)))

(define dest-moon-ifc nil)
       

(define (mk-moon tag name hours-per-phase hours-per-rev arc phase ifc gates color)
  (bind-astral-body (kern-mk-astral-body 
                     tag                          ; tag
                     name                         ; name
                     2                            ; relative distance
                     (* hours-per-phase 60)       ; minutes per phase
                     (/ (* hours-per-rev 60) 360) ; minutes per degree
                     arc                          ; initial arc
                     phase                        ; initial phase
                     ifc                          ; script interface
                     ;; phase sprites
                     (cond ((string=? color "yellow")
                            (list 
                             (list s_yellow_new_moon                0   "new")
                             (list s_yellow_wax_quarter_moon        16  "1/4 waxing")
                             (list s_yellow_wax_half_moon           32  "1/2 waxing")
                             (list s_yellow_wax_three_quarter_moon  64  "3/4 waxing")
                             (list s_yellow_full_moon               96 "full")
                             (list s_yellow_wane_three_quarter_moon 64  "3/4 waning")
                             (list s_yellow_wane_half_moon          32  "1/2 waning")
                             (list s_yellow_wane_quarter_moon       16  "1/4 waning")))
                           ((string=? color "blue")
                            (list 
                             (list s_blue_new_moon                0   "new")
                             (list s_blue_wax_quarter_moon        16  "1/4 waxing")
                             (list s_blue_wax_half_moon           32  "1/2 waxing")
                             (list s_blue_wax_three_quarter_moon  64  "3/4 waxing")
                             (list s_blue_full_moon               96 "full")
                             (list s_blue_wane_three_quarter_moon 64  "3/4 waning")
                             (list s_blue_wane_half_moon          32  "1/2 waning")
                             (list s_blue_wane_quarter_moon       16  "1/4 waning")))
                           (else
                            (list 
                             (list s_new_moon                0   "new")
                             (list s_wax_quarter_moon        16  "1/4 waxing")
                             (list s_wax_half_moon           32  "1/2 waxing")
                             (list s_wax_three_quarter_moon  64  "3/4 waxing")
                             (list s_full_moon               96 "full")
                             (list s_wane_three_quarter_moon 64  "3/4 waning")
                             (list s_wane_half_moon          32  "1/2 waning")
                             (list s_wane_quarter_moon       16  "1/4 waning")))))
                    gates))
