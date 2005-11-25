;;;;
;;;; old-mine.scm -- an abandoned mine littered with gems
;;;;

(mk-dungeon-room
 'p_old_mine "Old Mine"
 (list
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
      "rr rr bb bb rr rr rr rr rr bb bb bb .. .. .. .. bb bb rr "
      "rr bb .. .. bb rr rr rr bb .. .. .. .. .. .. .. .. bb rr "
      "rr bb .. .. .. .. .. .. .. .. .. .. rr rr .. .. .. bb rr "
      "rr rr .. .. .. .. .. .. .. .. .. bb rr rr rr .. .. rr rr "
      "rr rr .. .. rr rr rr rr .. .. .. rr rr rr rr .. .. rr rr "
      "rr rr .. .. bb rr rr rr rr .. .. bb bb rr rr .. .. rr rr "
      "rr rr .. .. .. bb bb bb bb .. .. .. bb .. bb .. .. rr rr "
      "rr rr .. .. .. bb bb .. .. .. .. .. .. .. .. .. .. rr rr "
      "rr rr .. .. bb rr .. .. .. .. .. .. .. .. .. .. .. rr rr "
      "rr rr .. .. rr bb .. .. .. .. .. rr rr .. .. .. .. bb rr "
      "rr rr .. .. rr rr .. .. .. .. bb rr rr rr rr .. .. rr rr "
      "rr rr .. .. .. bb .. .. rr bb rr rr rr rr rr .. .. rr rr "
      "rr rr .. .. .. .. .. rr rr rr rr .. .. rr .. .. .. rr rr "
      "rr bb .. .. .. .. .. rr .. .. .. .. .. .. .. .. .. rr rr "
      "rr bb .. .. .. .. .. .. .. .. .. .. .. .. .. rr .. rr rr "
      "rr rr bb .. .. .. .. .. .. rr rr .. .. rr rr rr .. .. rr "
      "rr rr rr rr bb rr rr rr rr rr rr rr rr rr rr rr .. .. rr "
      "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 (put (mk-ladder-down 'p_trolls_den 3 15) 17 17)

 ;; put some wandering ghasts in
 (put (guard-pt 'skeletal-warrior) 4 15)
 (put (guard-pt 'skeletal-warrior) 15 2)
 (put (guard-pt 'skeletal-spear-thrower) 9 9)
 (put (spawn-pt 'ghast) 15  9)
 (put (spawn-pt 'ghast) 11 14)
 (put (spawn-pt 'ghast)  9  4)
 (put (spawn-pt 'giant-spider) 3 3)
 (put (spawn-pt 'giant-spider) 4 4)
 
 (put (mk-step-trig 'wind-trap nil) 15 14)
 (put (mk-step-trig 'wind-trap nil) 16 12)
 (put (mk-step-trig 'wind-trap nil)  2  8)
 (put (mk-step-trig 'wind-trap nil) 10  3)
 )

;; drop gems
(put-random-stuff p_old_mine
                  (mk-rect 0 0 19 19)
                  (lambda (loc)
                    (eqv? (kern-place-get-terrain loc)
                          t_boulder))
                  (lambda (loc)
                    (kern-obj-put-at (kern-mk-obj t_gem 
                                                  (modulo (random-next) 
                                                          10))
                                     loc))
                  10)

;; drop some corpses
(drop-random-corpses p_old_mine 5)

;; fill upper left corner with cobwebs
(webify p_old_mine 0 0 6 7)
