
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

(define (map-paste-centered dstmap srcmap sx sy sw sh dx dy dw dh)
	(let* ((cw (min sw dw))
		(ch (min sh dh))
		(scx (floor (+ sx (/ (- sw cw) 2))))
		(dcx (floor (+ dx (/ (- dw cw) 2))))
		(scy (floor (+ sy (/ (- sh ch) 2))))
		(dcy (floor (+ dy (/ (- dh ch) 2)))))
		(kern-blit-map dstmap dcx dcy
			srcmap scx scy
			cw ch)
	))





(kern-mk-map 
 'm_ship_n 9 17 pal_expanded
 (list
  "-- -- -- -- -- -- -- -- --";
  "-- -- -- #e #a #f -- -- --";
  "-- -- #e #E ee #F #f -- --";
  "-- #e #E ee ee ee #F #f --";
  "-- #b ee ee ee ee ee #c --";
  "-- #b <n #= #= #= <n #c --";
  "ee ee ee ee oo ee ee ee ee";
  "-- #b ee ee ee ee ee #c --";
  "-- #b ee ee ee ee ee #c --";
  "-- #b ee ee ee ee ee #c --";
  "ee ee ee ee ee ee ee ee ee";
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
  "-- #b ee ee oo ee ee #c --";
  "-- #b <n #= #= #= <n #c --";
  "-- #b ee ee oo ee ee #c --";
  "ee ee ee ee ee ee ee ee ee";
  "-- #b ee ee ee ee ee #c --";
  "-- #b ee ee ee ee ee #c --";
  "-- #b ee ee ee ee ee #c --";
  "ee ee ee ee oo ee ee ee ee";
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
  "-- -- -- -- -- -- ee -- -- -- ee -- -- -- -- -- --";
  "-- -- #e #a #a #a ee #a #a #a ee #a #a #f -- -- --";
  "-- #e #E ee <w ee ee ee ee ee ee <e ee #F #f -- --";
  "-- #b ee ee #| ee ee ee ee ee ee #| ee ee #F #f --";
  "-- #b ee ee #| oo ee ee ee ee oo #| ee ee ee #c --";
  "-- #b ee ee #| ee ee ee ee ee ee #| ee ee #H #h --";
  "-- #g #G ee <w ee ee ee ee ee ee <e ee #H #h -- --";
  "-- -- #g #d #d #d ee #d #d #d ee #d #d #h -- -- --";
  "-- -- -- -- -- -- ee -- -- -- ee -- -- -- -- -- --";
  ))
  
(kern-mk-map 
 'm_ship_w 17 9 pal_expanded
 (list
  "-- -- -- -- -- -- ee -- -- -- ee -- -- -- -- -- --";
  "-- -- -- #e #a #a ee #a #a #a ee #a #a #a #f -- --";
  "-- -- #e #E ee <w ee ee ee ee ee ee <e ee #F #f --";
  "-- #e #E ee ee #| ee ee ee ee ee ee #| ee ee #c --";
  "-- #b ee ee ee #| oo ee ee ee ee oo #| ee ee #c --";
  "-- #g #G ee ee #| ee ee ee ee ee ee #| ee ee #c --";
  "-- -- #g #G ee <w ee ee ee ee ee ee <e ee #H #h --";
  "-- -- -- #g #d #d ee #d #d #d ee #d #d #d #h -- --";
  "-- -- -- -- -- -- ee -- -- -- ee -- -- -- -- -- --";
  ))

(define vehicle-ship-handler
	(lambda (place vehicle off_x off_y map_x map_y)
		(let* ((facing (vehicle-broadside-facing vehicle off_x off_y))
			(vmap (get-cardinal-lref (list m_ship_n m_ship_w m_ship_e m_ship_s) facing)))
		(map-paste-centered (kern-place-get-terrain-map place) vmap 0 0 
			(kern-terrainmap-get-width vmap) 
			(kern-terrainmap-get-height vmap) 
			(combat-off-to-dst off_x)
			(combat-off-to-dst off_y)
			(combat-off-to-len (kern-place-get-width place) off_x)
			(combat-off-to-len (kern-place-get-height place) off_y)	)		
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
                      2           ; tailwind penalty
                      4           ; headwind penalty
                      1           ; crosswind penalty
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
  "** #B ee ee ## ee ee #C **"
  "** #B ee ee ## ee ee #C **"
  "** #B ee ee ee ee ee #C **"
  "ee ee ee ee ee ee ee ee ee"
  "** #B ee ee ee ee ee #C **"
  "#> #> #> ee ee ee #> #> #>"
  "#> #> #> ee ## ee #> #> #>"
  "#> #> #> ee ## ee #> #> #>"
  "#> #> #> #D ## #D #> #> #>"
  "#> #> #> ** ** ** #> #> #>"
  "#> #> #> ** ** ** #> #> #>"
  "** ** ** ** ** ** ** ** **"
  ))
  
(kern-mk-map 
 'm_voidship_s 9 17 pal_expanded
 (list
  "** ** ** ** ** ** ** ** **"
  "#> #> #> ** ** ** #> #> #>"
  "#> #> #> ** ** ** #> #> #>"
  "#> #> #> #A ## #A #> #> #>"
  "#> #> #> ee ## ee #> #> #>"
  "#> #> #> ee ## ee #> #> #>"
  "#> #> #> ee ee ee #> #> #>"
  "** #B ee ee ee ee ee #C **"
  "ee ee ee ee ee ee ee ee ee"
  "** #B ee ee ee ee ee #C **"
  "** #B ee ee ## ee ee #C **"
  "** #B ee ee ## ee ee #C **"
  "** #k #G ee ee ee #H #l **"
  "** ** #k #G ee #H #l ** **"
  "** ** ** #k #D #l ** ** **"
  "** ** ** ** ** ** ** ** **"
  "** ** ** ** ** ** ** ** **"
  ))
  
(kern-mk-map 
 'm_voidship_e 17 9 pal_expanded
 (list
  "** #> #> #> #> #> #> ** ee ** ** ** ** ** ** ** **";
  "** #> #> #> #> #> #> #A ee #A #A #A #j ** ** ** **";
  "** #> #> #> #> #> #> ee ee ee ee ee #F #j ** ** **";
  "** ** ** #B ee ee ee ee ee ee ee ee ee #F #j ** **";
  "** ** ** ## ## ## ee ee ee ee ## ## ee ee #C ** **";
  "** ** ** #B ee ee ee ee ee ee ee ee ee #H #l ** **";
  "** #> #> #> #> #> #> ee ee ee ee ee #H #l ** ** **";
  "** #> #> #> #> #> #> #D ee #D #D #D #l ** ** ** **";
  "** #> #> #> #> #> #> ** ee ** ** ** ** ** ** ** **";
  ))
  

(kern-mk-map 
 'm_voidship_w 17 9 pal_expanded
 (list
  "** ** ** ** ** ** ** ** ee ** #> #> #> #> #> #> **";
  "** ** ** ** #i #A #A #A ee #A #> #> #> #> #> #> **";
  "** ** ** #i #E ee ee ee ee ee #> #> #> #> #> #> **";
  "** ** #i #E ee ee ee ee ee ee ee ee ee #C ** ** **";
  "** ** #B ee ee ## ## ee ee ee ee ## ## ## ** ** **";
  "** ** #k #G ee ee ee ee ee ee ee ee ee #C ** ** **";
  "** ** ** #k #G ee ee ee ee ee #> #> #> #> #> #> **";
  "** ** ** ** #k #D #D #D ee #D #> #> #> #> #> #> **";
  "** ** ** ** ** ** ** ** ee ** #> #> #> #> #> #> **";
  ))
    
(define vehicle-voidship-handler
	(lambda (place vehicle off_x off_y map_x map_y)
		(let* ((facing (vehicle-broadside-facing vehicle off_x off_y))
			(vmap (get-cardinal-lref (list m_voidship_n m_voidship_w m_voidship_e m_voidship_s) facing)))
		(map-paste-centered (kern-place-get-terrain-map place) vmap 0 0 
			(kern-terrainmap-get-width vmap) 
			(kern-terrainmap-get-height vmap) 
			(combat-off-to-dst off_x)
			(combat-off-to-dst off_y)
			(combat-off-to-len (kern-place-get-width place) off_x)
			(combat-off-to-len (kern-place-get-height place) off_y)	)		
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
                      2           ; tailwind penalty
                      4           ; headwind penalty
                      1           ; crosswind penalty
                      100         ; max hp
                      speed-ship  ; speed
                      mmode-voidship  ; pmask
                      vehicle-voidship-handler;
                      )


(define (mk-voidship)
  (kern-mk-vehicle t_voidship north 100))

(define (mk-vehicle ktype)
  (kern-mk-vehicle ktype north 100))
