(kern-mk-place 
	'p_voidgap_entrance
	"Passage to Voidgap"
	s_dungeon          ; sprite
	(kern-mk-map nil 19 19 pal_expanded
	(list
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn r8 rn rn rn rn "
		"rn rn r8 r8 r8 r8 r8 rn r8 rn rn rn rn rc {{ ra r8 r8 rn "
		"rn rc {4 {{ {{ {{ {2 re bb ra r8 xx rc {{ {{ {2 .. .. ra "
		"rc .. {4 {{ {{ {3 .. .. .. .. {4 xx {{ {{ {{ {2 .. .. .! "
		".. .. .. {1 {1 .. .. .. .. .. .. x! {5 {{ {3 .. .. .! .. "
		".. .. .. .. .. .. .. .. .. .. .. w+ .. {1 .. .. .. .. .! "
		".. .. .. .. .. bb .. .. .. .. .. .. .. .. bb .. .. .. .. "
		".. .. .. .. .. .. .. .. {8 .. .. w+ .. .. .. .. .. .. .! "
		".. .. .. .. .. .. {8 {c {{ {2 .. .. .. .. .. .. .. .! .. "
		".. .. bb .. .. {c {{ {{ {{ {2 .. w+ .. .. .. .. .. {8 .! "
		".. .. .. .. {c {{ {{ rf {{ {2 .. x! .. .. .. .. {4 {{ {a "
		".. .. .. {4 {{ {{ {{ {{ {3 .. .. xx .. .. .. .. {4 {{ {{ "
		".. r7 .. .. r3 r5 {{ {{ {2 {8 {8 xx .. r3 r5 .. .. {5 {{ "
		"r1 rn r1 r1 rn rn r1 r5 {4 {{ {{ xx r3 rn rn r1 r1 r1 r1 "
		"rn rn rn rn rn rn rn rn r5 {{ {{ rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn r1 r1 rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "

	)	
	)

	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	 (list ; objects
		(put (mk-monman) 0 0)
		
		 (put (mk-windowed-door) 11 8)
 		(put (mk-windowed-door)  11 10)
		
		 (put (guard-pt 'knight)  13 8)
		 (put (guard-pt 'knight)  13 10)
		 (put (guard-pt 'halberdier)  14 9)
		 (put (guard-pt 'crossbowman)  15 10)
		 (put (guard-pt 'crossbowman)  15 8)
		 
		(put (spawn-pt 'cave-goblin-slinger-m) 3 7)
		(put (spawn-pt 'cave-goblin-berserker-m) 6 12)
		(put (spawn-pt 'cave-goblin-slinger-m) 4 11)
		(put (spawn-pt 'cave-goblin-berserker-m) 7 8)
		(put (spawn-pt 'troll-m) 2 8)
	 )

	 (list
		 'on-entry-to-dungeon-room
	 	'voidgap-room-handle-start
		) ;; hooks
 (list  ;; edge entrances
  (list northwest 18 14)
  (list southwest 18 6)
  )
 )

(mk-place-music p_voidgap_entrance 'ml-castle)
 
 
(kern-mk-place 
	'p_voidgap_exit
	"Tunnels at Voidgap"
	s_dungeon          ; sprite
	(kern-mk-map nil 19 19 pal_expanded
	(list
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn r8 r8 rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rc .. .. ra r8 r8 rn rn rn r8 r8 r8 r8 r8 r8 r8 rn "
		"r8 rc .. .. {4 {{ {{ {{ ra r8 rc {{ {{ {{ {2 bb .. .. ra "
		".! .. .. .. .. {1 {5 {{ {{ {{ {{ {{ {{ {{ {2 .. .. .. .. "
		".! .! .. .. .. .. .. {1 {5 {{ {{ rf {{ {3 .. .. .. .. .. "
		".. .. .. .. .. .. .. bb .. {1 {5 {{ {3 .. .. .. .. .. .. "
		".! .. .. .. .. .. .. .. .. .. .. {1 .. .. .. .. .. .. .. "
		".! .. .. .. .. .. .. .. .. .. .. .. .. .. bb .. .. .. .. "
		".. .! .. bb .. .. .. .. .. rf .. .. .. .. .. .. .. .. .. "
		".! .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		"{8 {8 {8 .. .. .. .. bb .. .. .. .. .. .. .. .. .. .. .. "
		"{{ {{ {{ {2 .. .. .. .. .. .. .. .. .. .. .. .. bb .. .. "
		"{{ {3 {1 .. .. r3 r5 .. .. .. r3 r5 .. .. .. r3 r5 .. .. "
		"r5 .. r3 r1 r1 rn rn r1 r1 r1 rn rn r1 r1 r1 rn rn r1 r1 "
		"rn r1 rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "

	)	
	)

	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	 nil ; objects
 
	 (list
	 	'voidgap-room-handle-end
		) ;; hooks
 (list  ;; edge entrances
  (list northeast 0 14)
  (list southeast 0 5)
  )
 )

(mk-place-music p_voidgap_exit 'ml-dungeon-adventure)


(kern-mk-place 
	'p_voidgap_1
	"Voidgap Tunnels"
	 nil          ; sprite
	(kern-mk-map nil 19 19 pal_expanded
		(list
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn rc .. .. .. .. .. .. .. .. .. ra rn rn rn rn "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"r8 r8 rc .. .. .. .. .. .. .. .. .. .. .. .. .. ra r8 r8 "
		".. .. .. .. .. .. .A .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		"r1 r1 r5 .. .. .. .. .. .. .. .. .. .. .. .. .. r3 r1 r1 "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"rn rn rn rn r5 .. .. .. .. .. .. .. .. .. r3 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		)
	)
	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	nil; objects
	 
	 (list
		;'on-entry-to-dungeon-room
		'voidgap-room-handle-deeps
	) ;; hooks
	nil
)

 
 (kern-mk-place 
	'p_voidgap_2
	"Voidgap Tunnels"
	nil          ; sprite
	(kern-mk-map nil 19 19 pal_expanded
		(list
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn rc .. .. .. .. .. .. .. .. .. ra rn rn rn rn "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"r8 r8 rc .. .. .. .. .. .. .. .. .. .. .. .. .. ra r8 r8 "
		".. .. .. .. .. .. .A .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		"r1 r1 r5 .. .. .. .. .. .. .. .. .. .. .. .. .. r3 r1 r1 "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"rn rn rn rn r5 .. .. .. .. .. .. .. .. .. r3 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		)
	)
	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	 nil
 
	 (list
		;'on-entry-to-dungeon-room
		'voidgap-room-handle-deeps
	) ;; hooks
	nil
)
 
   (kern-mk-place 
	'p_voidgap_3
	"Voidgap Tunnels"
	 nil          ; sprite
	(kern-mk-map nil 19 19 pal_expanded
		(list
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn rc .. .. .. .. .. .. .. .. .. ra rn rn rn rn "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"r8 r8 rc .. .. .. .. .. .. .. .. .. .. .. .. .. ra r8 r8 "
		".. .. .. .. .. .. .A .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		"r1 r1 r5 .. .. .. .. .. .. .. .. .. .. .. .. .. r3 r1 r1 "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"rn rn rn rn r5 .. .. .. .. .. .. .. .. .. r3 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		)
	)
	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	 nil
	 
	 (list
		;'on-entry-to-dungeon-room
		'voidgap-room-handle-deeps
	) ;; hooks
 	nil
 )
 
   (kern-mk-place 
	'p_voidgap_4
	"Voidgap Tunnels"
	 nil          ; sprite
	(kern-mk-map nil 19 19 pal_expanded
		(list
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn rc .. .. .. .. .. .. .. .. .. ra rn rn rn rn "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"r8 r8 rc .. .. .. .. .. .. .. .. .. .. .. .. .. ra r8 r8 "
		".. .. .. .. .. .. .A .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		"r1 r1 r5 .. .. .. .. .. .. .. .. .. .. .. .. .. r3 r1 r1 "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"rn rn rn rn r5 .. .. .. .. .. .. .. .. .. r3 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		)
	)
	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	 nil
	 
	 (list
		;'on-entry-to-dungeon-room
		'voidgap-room-handle-deeps
	) ;; hooks
 	nil
 )
 
 (kern-mk-place 
	'p_voidgap_5
	"Voidgap Tunnels"
	  nil          ; sprite
	(kern-mk-map nil 19 19 pal_expanded
		(list
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn rc .. .. .. .. .. .. .. .. .. ra rn rn rn rn "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"r8 r8 rc .. .. .. .. .. .. .. .. .. .. .. .. .. ra r8 r8 "
		".. .. .. .. .. .. .A .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		"r1 r1 r5 .. .. .. .. .. .. .. .. .. .. .. .. .. r3 r1 r1 "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"rn rn rn rn r5 .. .. .. .. .. .. .. .. .. r3 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		)
	)
	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	 nil
	 
	 (list
		;'on-entry-to-dungeon-room
		'voidgap-room-handle-deeps
	) ;; hooks
	nil
 )


(mk-place-music p_voidgap_1 'ml-dungeon-adventure)
(mk-place-music p_voidgap_2 'ml-dungeon-adventure)
(mk-place-music p_voidgap_3 'ml-dungeon-adventure)
(mk-place-music p_voidgap_4 'ml-dungeon-adventure)
(mk-place-music p_voidgap_5 'ml-dungeon-adventure)

(prmap-linkrooms-2d 'p_voidgap_1 'p_voidgap_2 'p_voidgap_3 'p_voidgap_4 'p_voidgap_5)

(prmap-set-mapdata p_voidgap_1 (prmap-mk-mapdata "voidgap" voidgap-random-type-ns voidgap-random-type-ew voidgap-random-type-area 'deep-terrain-edges 'deep-terrain-area 'deep-room-blitstats 'voidgap-room-custom-hardlinks))

(let ((voidgap-hardlinks (prmap-params-hardlinks (prmap-get-mapdata p_voidgap_1))))
	(define (link-rm xloc yloc zloc dir target maptemplate passable )
		(prmap-room-hardlink-set! xloc yloc zloc voidgap-hardlinks dir target maptemplate passable nil)
		)
		
;;---------------------------------------------------------
;; hardlink setup
;; 
;;			x	y	z	dir		target				template				passable

(link-rm	1	0	0	west	nil					'm_deeptempl_wall		#f)
(link-rm	0	1	0	south	nil					'm_deeptempl_wall		#f)
(link-rm	-1	0	0	east	'p_voidgap_entrance	'm_deeptempl_passage	#f)
(link-rm	0	-1	0	north	nil					'm_deeptempl_wall		#f)

(link-rm	-7	-5	0	east	nil					'm_deeptempl_wall		#f)
(link-rm	-6	-4	0	south	nil					'm_deeptempl_wall		#f)
(link-rm	-5	-5	0	west	'p_voidgap_exit	'm_deeptempl_passage	#f)
(link-rm	-6	-6	0	north	nil					'm_deeptempl_wall		#f)
)

;;flag for checking if cohesion check still needs to be performed
(mutable-list-set (prmap-get-mapdata p_voidgap_1) 10 #t)