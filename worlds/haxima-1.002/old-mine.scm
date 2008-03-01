;;;;
;;;; old-mine.scm -- an abandoned mine littered with gems
;;;;

(mk-dungeon-room
 'p_old_mine "Old Mine"
 (list
		"rn rn r8 r8 rn rn rn rn rn r8 r8 r8 r8 r8 r8 r8 r8 r8 rn "
		"rn rc bb bb ra rn rn rn rc bb bb bb .. .. .. .. bb bb r2 "
		"r4 bb .. .. bb ra r8 rc bb .. .. .. .. .. .. .. .. bb r2 "
		"r4 bb .. .. .. .. .. .. .. .. .. .. r3 r5 .. .. .. bb r2 "
		"rn r5 .. .. .. .. .. .. .. .. .. bb r2 rn r5 .. .. r3 rn "
		"rn r4 .. .. rb r1 r1 r5 .. .. .. rb r8 rn r4 .. .. r2 rn "
		"rn r4 .. .. bb ra r8 r8 rd .. .. bb bb ra rc .. .. r2 rn "
		"rn r4 .. .. .. bb bb bb bb .. .. .. bb .. bb .. .. r2 rn "
		"rn r4 .. .. .. bb bb .. .. .. .. .. .. .. .. .. .. r2 rn "
		"rn r4 .. .. bb rf .. .. .. .. .. .. .. .. .. .. .. ra rn "
		"rn r4 .. .. r7 bb .. .. .. .. .. r3 r5 .. .. .. .. bb r2 "
		"rn r4 .. .. ra rd .. .. .. .. bb r2 rn r1 r5 .. .. r3 rn "
		"rn r4 .. .. .. bb .. .. r7 bb r3 r8 r8 rn rc .. .. r2 rn "
		"rn rc .. .. .. .. .. r3 r8 r9 rc .. .. re .. .. .. r2 rn "
		"r4 bb .. .. .. .. .. re .. .. .. .. .. .. .. .. .. r2 rn "
		"r4 bb .. .. .. .. .. .. .. .. .. .. .. .. .. r7 .. ra rn "
		"rn r5 bb .. .. .. .. .. .. r3 r5 .. .. r3 r1 r4 .. .. r2 "
		"rn rn r1 r5 bb r3 r1 r1 r1 rn rn r1 r1 rn rn r4 .. .. r2 "
		"rn rn rn rn r1 rn rn rn rn rn rn rn rn rn rn rn r1 r1 rn "
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

(mk-place-music p_old_mine 'ml-dungeon-adventure)
