
;;---------------------------------------------------------------
;; utility stuff

(define (vehicle-broadside-facing vehicle dx dy)
	(let ((facing (kern-obj-get-facing vehicle)))
		(cond ((< dx 0) ;;west side
					(if (equal? facing NORTH) NORTH SOUTH))
				((> dx 0) ;;east side
					(if (equal? facing SOUTH) SOUTH NORTH))
				((< dy 0) ;;north side
					(if (equal? facing WEST) WEST EAST))
				((> dy 0) ;;south side
					(if (equal? facing EAST) EAST WEST))
				(else facing)
		)))

(define (map-paste-centered dst-map src-map src-x src-y src-w src-h dst-x dst-y dst-w dst-h)
	(let* ((cw (min src-w dst-w))
		(ch (min src-h dst-h))
		(src-cx (floor (+ src-x (/ (- src-w cw) 2))))
		(dst-cx (floor (+ dst-x (/ (- dst-w cw) 2))))
		(src-cy (floor (+ src-y (/ (- src-h ch) 2))))
		(dst-cy (floor (+ dst-y (/ (- dst-h ch) 2)))))
		(kern-blit-map dst-map dst-cx dst-cy
			src-map src-cx src-cy
			cw ch)
	))
	
(define (place-add-objects-offset place src-x src-y src-w src-h dst-x dst-y dst-w dst-h objectlist)
	(let* ((cw (min src-w dst-w))
		(ch (min src-h dst-h))
		(src-cx (floor (+ src-x (/ (- src-w cw) 2))))
		(dst-cx (floor (+ dst-x (/ (- dst-w cw) 2))))
		(src-cy (floor (+ src-y (/ (- src-h ch) 2))))
		(dst-cy (floor (+ dst-y (/ (- dst-h ch) 2))))
		(dx (- dst-cx src-cx))
		(dy (- dst-cy src-cy)))
		(map
			(lambda (objectentry)
				(kern-obj-put-at (car objectentry) 
					(list place
						(+ (cadr objectentry) dx)
						(+ (caddr objectentry) dy)
					))
			)
			objectlist
		)
	))
	
(define (mk-vehicle ktype)
  (kern-mk-vehicle ktype north 100))
  
(define (vehicle-object-list-rotate facing n-wid n-hgt objectlist)
	(let* ((turn-matrix (cond 
				((equal? facing NORTH) (list 0 0 1 0 0 1))
				((equal? facing EAST) (list (- n-hgt 1) 0 0 -1 1 0))
				((equal? facing WEST) (list 0 (- n-wid 1) 0 1 -1 0))
				(else (list (- n-wid 1) (- n-hgt 1) -1 0 0 -1))))
			(xoff (car turn-matrix))
			(yoff (cadr turn-matrix))
			(xxmult (list-ref turn-matrix 2))
			(xymult (list-ref turn-matrix 3))
			(yxmult (list-ref turn-matrix 4))
			(yymult (list-ref turn-matrix 5)))
			(map	
				(lambda (objectentry)
					(list
						(car objectentry)
						(+ xoff (* xxmult (cadr objectentry)) (* xymult (caddr objectentry)))
						(+ yoff (* yxmult (cadr objectentry)) (* yymult (caddr objectentry)))
					)
				)
				objectlist
			)
	))

(define (facing-turn-90right facing)
	(cond ((equal? facing NORTH) EAST)
		((equal? facing WEST) NORTH)
		((equal? facing EAST) SOUTH)
		((equal? facing SOUTH) WEST)
		(else facing)))

(define (facing-turn-90left facing)
	(cond ((equal? facing NORTH) WEST)
		((equal? facing WEST) SOUTH)
		((equal? facing EAST) NORTH)
		((equal? facing SOUTH) EAST)
		(else facing)))

		
;;--------------------------------------------------------------------------
;; vehicle objects: wheel
	
(define shipwheel-ifc
	(ifc '()
		(method 'init
			(lambda (kwheel)
				(kern-obj-set-facing kwheel (gob kwheel))
		))	
	))

(mk-obj-type 't_shipswheel "ship's wheel" s_shipswheel layer-mechanism shipwheel-ifc)     
         
(define  (vehicle-mk-wheel facing)
	(let ((kwheel (kern-mk-obj t_shipswheel 1)))
          (kern-obj-set-facing kwheel facing) 
          (bind kwheel facing)
          kwheel))

;;---------------------------------------------------------------------------
;; boarding ramp handling

(define onramp-ifc
	(ifc '()
		(method 'exec
			(lambda (kramp)
				(let* ((kloc (kern-obj-get-location kramp))
					(kplace (car kloc))
					(wid (kern-place-get-width kplace))
					(hgt (kern-place-get-height kplace))
					(rx (cadr kloc))
					(ry (caddr kloc)))
					(define (vehicle-check-ramp x y)
						(cond ((< x 0) 0)
							((> x wid) 0)
							((< y 0) 0)
							((> y hgt) 0)
							((kern-place-is-passable (list kplace x y) kramp) 1)
							(else 
								(let ((objs (kern-get-objects-at (list kplace x y))))
									(if (and (not (null? objs))
											(equal? (kern-obj-get-type (car objs)) (eval 't_onramp)))
										1
										0)))
						))
					(define (vehicle-trigger-ramp-neighbors x y)
						(cond ((< x 0) 0)
							((> x wid) 0)
							((< y 0) 0)
							((> y hgt) 0)
							(else 
								(let ((objs (kern-get-objects-at (list kplace x y))))
									(if (and (not (null? objs))
											(equal? (kern-obj-get-type (car objs)) (eval 't_onramp)))
										((kobj-ifc (car objs)) 'exec (car objs))
										0)))
						))
					(if (< 1
							(+ (vehicle-check-ramp (+ rx 1) ry)
								(vehicle-check-ramp (- rx 1) ry)
								(vehicle-check-ramp rx (+ ry 1))
								(vehicle-check-ramp rx (- ry 1))))
						(begin 
							(kern-place-set-terrain (list kplace rx ry) t_deck)
							(kern-obj-remove kramp)
							(vehicle-trigger-ramp-neighbors (+ rx 1) ry)
							(vehicle-trigger-ramp-neighbors (- rx 1) ry)
							(vehicle-trigger-ramp-neighbors  rx (+ ry 1))
							(vehicle-trigger-ramp-neighbors  rx (- ry 1))
						)
						(kern-obj-remove kramp)
					)
			)))
		))

(mk-obj-type 't_onramp nil nil layer-none onramp-ifc)

;;------------------------------------------------------------------------
;; ship
	
(kern-mk-map 
 'm_ship_n 9 17 pal_expanded
 (list
  "-- -- -- -- -- -- -- -- --";
  "-- -- -- #e #a #f -- -- --";
  "-- -- #e #E ee #F #f -- --";
  "-- #e #E ee ee ee #F #f --";
  "-- #b ee ee ee ee ee #c --";
  "-- #b <n #= #= #= <n #c --";
  "-- ee ee ee oo ee ee ee --";
  "-- #b ee ee ee ee ee #c --";
  "-- #b ee ee ee ee ee #c --";
  "-- #b ee ee ee ee ee #c --";
  "-- ee ee ee ee ee ee ee --";
  "-- #b ee ee oo ee ee #c --";
  "-- #b <s #= #= #= <s #c --";
  "-- #b ee ee ee ee ee #c --";
  "-- #g #G ee ee ee #H #h --";
  "-- -- #g #d #d #d #h -- --";
  "-- -- -- -- -- -- -- -- --";
  ))
  
(kern-mk-map 
 'm_ship_s 9 17 pal_expanded
 (list
  "-- -- -- -- -- -- -- -- --";
  "-- -- #e #a #a #a #f -- --";
  "-- #e #E ee ee ee #F #f --";
  "-- #b ee ee ee ee ee #c --";
  "-- #b <n #= #= #= <n #c --";
  "-- #b ee ee oo ee ee #c --";
  "-- ee ee ee ee ee ee ee --";
  "-- #b ee ee ee ee ee #c --";
  "-- #b ee ee ee ee ee #c --";
  "-- #b ee ee ee ee ee #c --";
  "-- ee ee ee oo ee ee ee --";
  "-- #b <s #= #= #= <s #c --";
  "-- #b ee ee ee ee ee #c --";
  "-- #g #G ee ee ee #H #h --";
  "-- -- #g #G ee #H #h -- --";
  "-- -- -- #g #d #h -- -- --";
  "-- -- -- -- -- -- -- -- --";
  ))
  
(kern-mk-map 
 'm_ship_e 17 9 pal_expanded
 (list
  "-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --";
  "-- -- #e #a #a #a ee #a #a #a ee #a #a #f -- -- --";
  "-- #e #E ee <w ee ee ee ee ee ee <e ee #F #f -- --";
  "-- #b ee ee #| ee ee ee ee ee ee #| ee ee #F #f --";
  "-- #b ee ee #| oo ee ee ee ee oo #| ee ee ee #c --";
  "-- #b ee ee #| ee ee ee ee ee ee #| ee ee #H #h --";
  "-- #g #G ee <w ee ee ee ee ee ee <e ee #H #h -- --";
  "-- -- #g #d #d #d ee #d #d #d ee #d #d #h -- -- --";
  "-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --";
  ))
  
(kern-mk-map 
 'm_ship_w 17 9 pal_expanded
 (list
  "-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --";
  "-- -- -- #e #a #a ee #a #a #a ee #a #a #a #f -- --";
  "-- -- #e #E ee <w ee ee ee ee ee ee <e ee #F #f --";
  "-- #e #E ee ee #| ee ee ee ee ee ee #| ee ee #c --";
  "-- #b ee ee ee #| oo ee ee ee ee oo #| ee ee #c --";
  "-- #g #G ee ee #| ee ee ee ee ee ee #| ee ee #c --";
  "-- -- #g #G ee <w ee ee ee ee ee ee <e ee #H #h --";
  "-- -- -- #g #d #d ee #d #d #d ee #d #d #d #h -- --";
  "-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --";
  ))
  
(define vehicle-ship-handler
	(lambda (place vehicle off_x off_y)
		(let* ((facing (vehicle-broadside-facing vehicle off_x off_y))
			(vmap (get-cardinal-lref (list m_ship_n m_ship_w m_ship_e m_ship_s) facing))
			(src-w (kern-terrainmap-get-width vmap))
			(src-h (kern-terrainmap-get-height vmap))
			(dst-x (combat-off-to-dst off_x))
			(dst-y (combat-off-to-dst off_y))
			(dst-w (combat-off-to-len (kern-place-get-width place) off_x))
			(dst-h (combat-off-to-len (kern-place-get-height place) off_y)))
		(map-paste-centered (kern-place-get-terrain-map place) vmap
			0 0 src-w src-h
			dst-x dst-y dst-w dst-h)	
		(place-add-objects-offset place
			0 0 src-w src-h
			dst-x dst-y dst-w dst-h
			(vehicle-object-list-rotate facing 9 17 
			(list
				(list (vehicle-mk-wheel facing) 4 12)
				(list (arms-mk-cannon (facing-turn-90right facing)) 7 8)
				(list (arms-mk-cannon (facing-turn-90left facing)) 1 8)
				(list (kern-mk-obj t_onramp 1) 0 6)
				(list (kern-mk-obj t_onramp 1) 8 6)
				(list (kern-mk-obj t_onramp 1) 0 10)
				(list (kern-mk-obj t_onramp 1) 8 10)
			)))
	)))
		
  
(kern-mk-vehicle-type 't_ship   ; tag
                      "ship"    ; name
                      s_ship    ; sprite
                      m_ship_n    ; map
                      t_cannon  ; ordnance
                      #t        ; vulnerable
                      #t        ; occupants die when destroyed
                      #t        ; must turn
                      "sail"    ; move description
                      sound-ship-move ; move sound
                      1           ; tailwind penalty
                      4           ; headwind penalty
                      2           ; crosswind penalty
                      100         ; max hp
                      speed-ship  ; speed
                      mmode-ship  ; pmask
                      vehicle-ship-handler;
                      )

(define (mk-ship)
  (kern-mk-vehicle t_ship north 100))
	
;;----------------------------------------------------------------------------
;; voidship
;;----------------------------------------------------------------------------
(kern-mk-map 
 'm_voidship_n 9 17 pal_expanded
 (list
  "** ** ** ** ** ** ** ** **"
  "** ** ** ** ** ** ** ** **"
  "** ** ** #i #A #j ** ** **"
  "** ** #i #E ee #F #j ** **"
  "** #i #E ee ee ee #F #j **"
  "** #B ee ee 00 ee ee #C **"
  "** #B ee ee 00 ee ee #C **"
  "** #B ee ee ee ee ee #C **"
  "** ee ee ee ee ee ee ee **"
  "** #B ee ee ee ee ee #C **"
  "#n #m #O ee ee ee #N #m #o"
  "#m #m #M ee 00 ee #m #m #M"
  "#m #m #M ee ## ee #m #m #M"
  "#m #m #M #D ## #D #m #m #M"
  "#m #m #M ** ** ** #m #m #M"
  "#p #M #q ** ** ** #p #M #q"
  "** ** ** ** ** ** ** ** **"
  ))
  
(kern-mk-map 
 'm_voidship_s 9 17 pal_expanded
 (list
  "** ** ** ** ** ** ** ** **"
  "#n #m #o ** ** ** #n #m #o"
  "#m #m #M ** ** ** #m #m #M"
  "#m #m #M #A ## #A #m #m #M"
  "#m #m #M ee ## ee #m #m #M"
  "#m #m #M ee 00 ee #m #m #M"
  "#p #M #Q ee ee ee #P #M #q"
  "** #B ee ee ee ee ee #C **"
  "** ee ee ee ee ee ee ee **"
  "** #B ee ee ee ee ee #C **"
  "** #B ee ee 00 ee ee #C **"
  "** #B ee ee 00 ee ee #C **"
  "** #k #G ee ee ee #H #l **"
  "** ** #k #G ee #H #l ** **"
  "** ** ** #k #D #l ** ** **"
  "** ** ** ** ** ** ** ** **"
  "** ** ** ** ** ** ** ** **"
  ))
  
(kern-mk-map 
 'm_voidship_e 17 9 pal_expanded
 (list
  "** #n #m #m #m #m #o ** ** ** ** ** ** ** ** ** **";
  "** #m #m #m #m #m #M #A ee #A #A #A #j ** ** ** **";
  "** #p #M #M #M #M #Q ee ee ee ee ee #F #j ** ** **";
  "** ** ** #B ee ee ee ee ee ee ee ee ee #F #j ** **";
  "** ** ** ## ## 00 ee ee ee ee 00 00 ee ee #C ** **";
  "** ** ** #B ee ee ee ee ee ee ee ee ee #H #l ** **";
  "** #n #m #m #m #m #O ee ee ee ee ee #H #l ** ** **";
  "** #m #m #m #m #m #M #D ee #D #D #D #l ** ** ** **";
  "** #p #M #M #M #M #q ** ** ** ** ** ** ** ** ** **";
  ))
  

(kern-mk-map 
 'm_voidship_w 17 9 pal_expanded
 (list
  "** ** ** ** ** ** ** ** ** ** #n #m #m #m #m #o **";
  "** ** ** ** #i #A #A #A ee #A #m #m #m #m #m #M **";
  "** ** ** #i #E ee ee ee ee ee #P #M #M #M #M #q **";
  "** ** #i #E ee ee ee ee ee ee ee ee ee #C ** ** **";
  "** ** #B ee ee 00 00 ee ee ee ee 00 ## ## ** ** **";
  "** ** #k #G ee ee ee ee ee ee ee ee ee #C ** ** **";
  "** ** ** #k #G ee ee ee ee ee #N #m #m #m #m #o **";
  "** ** ** ** #k #D #D #D ee #D #m #m #m #m #m #M **";
  "** ** ** ** ** ** ** ** ** ** #p #M #M #M #M #q **";
  ))
    
(define vehicle-voidship-handler
	(lambda (place vehicle off_x off_y)
		(let* ((facing (vehicle-broadside-facing vehicle off_x off_y))
			(vmap (get-cardinal-lref (list m_voidship_n m_voidship_w m_voidship_e m_voidship_s) facing))
			(src-w (kern-terrainmap-get-width vmap))
			(src-h (kern-terrainmap-get-height vmap))
			(dst-x (combat-off-to-dst off_x))
			(dst-y (combat-off-to-dst off_y))
			(dst-w (combat-off-to-len (kern-place-get-width place) off_x))
			(dst-h (combat-off-to-len (kern-place-get-height place) off_y)))
		(map-paste-centered (kern-place-get-terrain-map place) vmap
			0 0 src-w src-h
			dst-x dst-y dst-w dst-h)	
		(place-add-objects-offset place
			0 0 src-w src-h
			dst-x dst-y dst-w dst-h
			(vehicle-object-list-rotate facing 9 17 
			(list
				(list (vehicle-mk-wheel facing) 4 6)
				(list (arms-mk-cannon (facing-turn-90right facing)) 7 6)
				(list (arms-mk-cannon (facing-turn-90left facing)) 1 6)				
				(list (kern-mk-obj t_onramp 1) 0 8)
				(list (kern-mk-obj t_onramp 1) 8 8)
			)))	
	)))
  
(kern-mk-vehicle-type 't_voidship   ; tag
                      "voidship"    ; name
                      s_void_ship   ; sprite
                      m_voidship_n    ; map
                      t_cannon  ; ordnance
                      #t        ; vulnerable
                      #t        ; occupants die when destroyed
                      #t        ; must turn
                      "sail"    ; move description
                      sound-ship-move ; move sound
                      1           ; tailwind penalty
                      4           ; headwind penalty
                      2           ; crosswind penalty
                      100         ; max hp
                      speed-ship  ; speed
                      mmode-voidship  ; pmask
                      vehicle-voidship-handler;
                      )


(define (mk-voidship)
  (kern-mk-vehicle t_voidship north 100))

