;;----------------------------------------------------------------------------
;; Troll Cave
;;
;; Big underground complex; created by some civilized race, now a ruin
;; inhabited by trolls and other monsters.
;;----------------------------------------------------------------------------

(kern-load "warritrix.scm")
 
 
(kern-mk-place 
	'p_lost_halls_entrance
	"Gate to the Lost Halls"
	s_dungeon          ; sprite
	(kern-mk-map nil 21 21 pal_expanded
	(list
		"rn rn rn r4 {8 .. bb .. {8 .. .. .. .. .. .. {8 r2 rn rn rn rn "
		"rn rn rn r4 {{ bb .. {c {{ {2 .. .. .. bb {c {{ r2 rn rn rn rn "
		"rn rn rn r4 {{ {2 {4 {{ {{ bb .. .. .. {4 {{ rb r8 rn rn rn rn "
		"rn rn rn rn r5 .. .. {5 {{ {2 .. .. .. {4 {{ {{ {2 r2 rn rn rn "
		"rn rn rn rn rn rd .. .. {1 .. .. .. .. r7 {{ {{ {a r2 rn rn rn "
		"rn rn rn rn r4 .. .. bb .. .. .. .. .. ra rd {{ {{ r2 rn rn rn "
		"rn rn rn rn r8 rd {8 .. .. .. .. .. {4 {{ {{ {7 {{ r2 rn rn rn "
		"rn rn rn rc {{ {6 {{ {a .. .. .. .. .. {1 bb .. rb rn rn rn rn "
		"rn rn r4 {{ {3 {4 {{ {{ {2 .. .. .. .. .. .. {4 {{ r2 rn rn rn "
		"rn rn r4 {1 xx xx xx {{ {2 .. .. .. .. xx xx xx {{ r2 rn rn rn "
		"rn xx xx xx xx xx xx {1 .. .. .. .. .. xx xx xx xx xx xx rn rn "
		"^^ ^^ ^^ {{ xx xx xx .. .. .. .. .. .. xx xx xx {{ ^^ ^^ ^^ rn "
		"^^ ^^ ^^ {{ {a {8 .. .. .. .. .. .. .. .. {4 {{ {{ ^^ ^^ ^^ rn "
		"^^ {{ {{ {{ {{ {{ {2 .. .. .. .. .. tf {8 bb {{ {{ ^^ ^^ ^^ ^^ "
		"{{ {{ {C bb {{ {{ {2 .. .. .. .. {4 {{ {{ {{ {{ {{ {{ {{ ^^ ^^ "
		"{1 t3 t5 .. {9 {9 .. tf .. .. .. .. {1 {5 {{ bb {{ ^^ ^^ ^^ ^^ "
		"{8 ta tt bb {{ {{ {2 .. .. .. .. .. .. .. {5 {{ {{ {{ ^^ ^^ ^^ "
		"{{ t% te {# {{ {3 .. .. .. .. .. .. {8 {8 .. {1 tf {{ ^^ ^^ ^^ "
		"{{ {{ {e {{ {3 {8 .. bb .. .. .. {4 {{ {{ {a {8 {4 {{ {{ {{ {{ "
		"{{ {{ {{ {C t7 {A {2 .. .. .. .. .. {5 {{ {{ {{ {2 tf {{ {7 {{ "
		"{A {{ {3 tb tt td .. .. .. .. .. .. {4 {{ {{ {3 .. .. {1 {4 {C "

	)	
	)

	 #f              ; wraps
	 #f              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	 (list ; objects
		(put (mk-monman) 0 0)
		(put (spawn-pt 'cave-goblin-slinger-m) 8 7)
		(put (spawn-pt 'cave-goblin-berserker-m) 9 13)
		(put (spawn-pt 'cave-goblin-slinger-m) 12 8)
		(put (spawn-pt 'cave-goblin-berserker-m) 13 14)
		(put (spawn-pt 'troll-m) 10 10)
	 )
 
	 (list
	 	'losthalls-room-handle-start
		'on-entry-to-dungeon-room
		) ;; hooks
		(list ;; edge entrances
		)
 )
 
(kern-mk-place 
	'p_lost_halls_r1
	"The Lost halls"
	 nil          ; sprite
	(kern-mk-map nil 21 21 pal_expanded
		(list
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
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
		'losthalls-room-handle-deeps
	) ;; hooks
	nil
)

(kern-mk-place 
	'p_lost_halls_r2
	"The Lost halls"
	 nil          ; sprite
	(kern-mk-map nil 21 21 pal_expanded
		(list
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
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
		'losthalls-room-handle-deeps
	) ;; hooks
	nil
)

(kern-mk-place 
	'p_lost_halls_r3
	"The Lost halls"
	 nil          ; sprite
	(kern-mk-map nil 21 21 pal_expanded
		(list
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
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
		'losthalls-room-handle-deeps
	) ;; hooks
	nil
)

(kern-mk-place 
	'p_lost_halls_r4
	"The Lost halls"
	 nil          ; sprite
	(kern-mk-map nil 21 21 pal_expanded
		(list
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
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
		'losthalls-room-handle-deeps
	) ;; hooks
	nil
)

(kern-mk-place 
	'p_lost_halls_r5
	"The Lost halls"
	 nil          ; sprite
	(kern-mk-map nil 21 21 pal_expanded
		(list
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
  "rn rn rn rn ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rn rn rn rn "
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
		'losthalls-room-handle-deeps
	) ;; hooks
	nil
)

(kern-mk-place 
	'p_lost_halls_staird
	"The Great Stair"
	 nil          ; sprite
	(kern-mk-map nil 21 21 pal_expanded
	(list
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn r8 rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn xx xx xx bb xx xx r8 xx xx xx rn rn rn rn rn rn "
		"rn rn rn rn r8 rc .. .. .. .. .. .. .. .. ra r8 r8 rn rn rn rn "
		"rn rn rn r4 .. .. .. .. .. .. .. .. .. .. .. .. .. r2 rn rn rn "
		"rn rn r4 {{ {2 .. .. .. .. r2 rn rn r4 .. .. .. .. ra rn rn rn "
		"rn rn xx {1 .. .. .. r3 r1 rn rn rn rn r5 .. .. .. .. xx rn rn "
		"rn rn xx .. .. .. r3 rn xx d, d, d, xx rn r5 .. .. .. bb r2 rn "
		"rn rn xx bb .. .. r2 rn xx ,, ,, ,, xx rn rc .. .. .. xx rn rn "
		"rn r4 bb .. .. .. ra rn xx cc cc cc xx r4 .. .. .. .. xx rn rn "
		"rn rn r5 .. .. .. .. r2 xx ,, ,, ,, xx r4 .. .. .. .. xx rn rn "
		"rn rn xx bb .. .. .. r2 xx cc cc cc xx xx .. .. .. .. r2 rn rn "
		"rn rn xx .. .. .. .. xx xx ,, ,, ,, xx xx .. .. .. .. r2 rn rn "
		"rn rn r4 .. .. .. xx xx xx cc cc cc xx xx xx .. .. .. xx rn rn "
		"rn rn xx .. .. .. xx xx xx ,, ,, ,, xx xx xx .. .. {8 xx rn rn "
		"rn rn rn r5 .. .. .. .. .. .. .. .. .. .. .. .. {4 {{ xx rn rn "
		"rn rn rn r4 .. {8 {8 .. .. .. .. .. .. .. .. .. .. r2 rn rn rn "
		"rn rn rn rn r1 {{ {{ {2 .. .. .. .. .. .. .. r3 r1 rn rn rn rn "
		"rn rn rn rn rn r1 xx xx r1 r1 xx r1 xx xx xx xx rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
	)

	)
	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	(list
		(put (mk-monman) 0 0)
	 (put (mk-bump-door 'p_lost_halls_stairu 10 18) 9 7)
	 (put (mk-bump-door 'p_lost_halls_stairu 10 18) 10 7)
	 (put (mk-bump-door 'p_lost_halls_stairu 10 18) 11 7)
	 (put (mk-step-trig 'one-off-message "A huge stairway leads down into the darkness" "losthalls_staird") 9 15)
	 (put (mk-step-trig 'one-off-message "A huge stairway leads down into the darkness" "losthalls_staird") 10 15)
	 (put (mk-step-trig 'one-off-message "A huge stairway leads down into the darkness" "losthalls_staird") 11 15)
	  (put (spawn-pt 'giant-spider) 4 11)
	  (put (spawn-pt 'giant-spider) 5 10)
	  (put (spawn-pt 'giant-spider) 6 12)
	(put (spawn-pt 'gint-warrior) 15 11)
	(put (spawn-pt 'gint-warrior) 15 11)
	(put (spawn-pt 'gint-mage) 14 10)
	); objects
	
	 
	 (list
		'on-entry-to-dungeon-room
		'losthalls-room-handle-staird
	) ;; hooks
	nil
)

(kern-mk-place 
	'p_lost_halls_stairu
	"The Great Stair"
	 nil          ; sprite
	(kern-mk-map nil 21 21 pal_expanded
	(list
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn xx xx xx xx xx xx xx xx xx xx xx xx xx rn rn rn rn "
		"rn rn rn xx xx .. .. .. .. .. .. .. .. .. .. .. xx xx rn rn rn "
		"rn rn rn xx .. .. .. .. .. .. .. .. .. .. .. .. .. xx rn rn rn "
		"rn rn rn xx .. .. .. .. ,, .. ,, .. .. .. ,, .. .. xx rn rn rn "
		"rn rn rn xx .. .. .. .. .. .. .. .. ,, .. .. .. .. xx rn rn rn "
		"rn rn rn xx .. .. .. ,, ,, ,, ,, .. .. .. .. .. .. xx rn rn rn "
		"rn rn rn xx .. .. .. ,, .. .. .. ,, ,, ,, ,, .. .. xx rn rn rn "
		"rn rn rn xx .. .. xx ,, ,, ,, ,, ,, ,, ,, xx .. .. xx rn rn rn "
		"rn rn rn xx .. xx xx xx cc cc cc cc cc xx xx xx .. xx rn rn rn "
		"rn rn rn xx .. xx xx xx ,, ,, ,, ,, ,, xx xx xx .. xx rn rn rn "
		"rn rn rn xx .. .. xx xx cc cc cc cc cc xx xx .. .. xx rn rn rn "
		"rn rn rn xx .. .. xx xx xx ,, ,, ,, xx xx xx .. .. xx rn rn rn "
		"rn rn rn xx .. .. .. xx xx cc cc cc xx xx .. .. .. xx rn rn rn "
		"rn rn rn xx xx .. .. xx xx ,, ,, ,, xx xx .. .. xx xx rn rn rn "
		"rn rn rn rn xx xx xx xx xx cc cc cc xx xx xx xx xx rn rn rn rn "
		"rn rn rn rn rn rn rn rn xx ,, ,, ,, xx rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn xx d, d, d, xx rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn xx xx xx xx xx rn rn rn rn rn rn rn rn "
	)
	)
	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	(list
		(put (mk-monman) 0 0)
	 (put (mk-bump-door 'p_lost_halls_staird 10 8) 9 19)
	 (put (mk-bump-door 'p_lost_halls_staird 10 8) 10 19)
	 (put (mk-bump-door 'p_lost_halls_staird 10 8) 11 19)
		(put (spawn-pt 'cave-goblin-slinger-m) 11 8)
		(put (spawn-pt 'cave-goblin-berserker-m) 8 12)
		(put (spawn-pt 'troll-m) 9 10)
		(put (spawn-pt 'gint-warrior-m) 12 9)

	); objects
	
	 
	 (list
		'on-entry-to-dungeon-room
		'losthalls-room-handle-stairu
	) ;; hooks
	nil
)

(kern-mk-place 
	'p_lost_halls_graves
	"The Graveyard"
	 nil          ; sprite
	(kern-mk-map nil 21 21 pal_expanded
	(list
		"rn rn rn rn rn rn rn xx r8 rn rn r8 rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn xx bb ra rc bb ra xx rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn xx rd bb .. .. bb xx rn rn rn rn rn rn rn "
		"rn rn rn rn xx xx xx xx .. .. .. .. .. xx xx xx xx rn rn rn rn "
		"rn rn rn xx xx .. .. .. .. .. .. .. .. .. .. .. xx xx rn rn rn "
		"rn rn rn xx .. .. .. .. .. .. .. .. .. .. .. {8 .. xx rn rn rn "
		"rn rn rn xx .. .. .. @@ ,R @@ ,I @@ ,P bb {4 {{ {2 xx rn rn rn "
		"rn xx xx xx .. .. .. .. .. .. .. .. .. .. .. {1 .. xx xx xx rn "
		"r4 bb .. bb .. .. 00 .. 00 .. 00 .. 00 .. 00 .. .. .. bb ra rn "
		"rn r5 bb .. .. {4 {{ {6 {{ {6 {{ {e {{ {6 {{ {2 .. .. bb .. r2 "
		"rn rn rd .. .. {4 {{ {6 bb {6 {{ {{ {{ {6 {{ {2 .. .. bb r3 rn "
		"rn r4 bb .. .. .. {1 .. {1 .. {1 {1 {1 .. {1 .. .. bb .. ra rn "
		"rn rn r5 .. .. .. 00 .. 00 .. 00 .. bb .. 00 .. .. .. .. bb r2 "
		"rn xx xx xx .. {4 {{ {6 {{ {6 {{ {6 {{ {6 {{ {2 .. xx xx xx rn "
		"rn rn rn xx .. {c {{ {6 {{ {6 {{ {6 {{ {6 {{ {2 .. xx rn rn rn "
		"rn rn rn xx {4 {{ {3 .. {1 .. {1 .. {1 .. {1 .. .. xx rn rn rn "
		"rn rn rn xx xx {{ {2 .. .. .. .. .. bb .. bb .. xx xx rn rn rn "
		"rn rn rn rn xx xx xx xx bb .. .. .. bb xx xx xx xx rn rn rn rn "
		"rn rn rn rn rn rn rn xx .. .. bb .. r3 xx rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn xx bb r3 r5 bb r2 xx rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn xx r1 rn rn r1 rn xx rn rn rn rn rn rn rn "
	)
	)
	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	(list
		(put (mk-monman) 0 0)
		(put (spawn-pt 'lich) 11 9)
		(put (spawn-pt 'ghast) 7 11)
		(put (spawn-pt 'ghast) 13 9)
		(put (spawn-pt 'skeletal-warrior) 7 7)
		(put (spawn-pt 'skeletal-warrior) 10 15)
		(put (spawn-pt 'skeletal-spear-thrower) 15 8)
		(put (spawn-pt 'skeletal-spear-thrower) 9 13)
	); objects
	
	 
	 (list
		'on-entry-to-dungeon-room
		'losthalls-room-handle-croom
	) ;; hooks
	nil
)

(drop-random-corpses p_lost_halls_graves 5)

(kern-mk-place 
	'p_lost_halls_rshrine
	"The Broken Sactuary"
	 nil          ; sprite
	(kern-mk-map nil 21 21 pal_expanded
	(list
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn xx xx xx xx xx xx xx xx xx xx xx xx xx rn rn rn rn "
		"rn rn rn xx xx @@ @@ @@ @@ ,, ++ ,, @@ @@ @@ @@ xx xx rn rn rn "
		"rn rn rn xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, bb xx rn rn rn "
		"rn rn rn xx ,, ~~ ~~ ~~ ,, ,, ,, ,, ,, ~~ ~~ ~~ ,, xx rn rn rn "
		"rn xx xx xx ,, b~ _! ~~ ,, ,, aa ,, ,, ~~ _! ~~ ,, xx xx xx rn "
		"r4 bb .. bb ,, ~~ ~~ ~~ ,, ,, ,, ,, ,, ~~ ~~ ~~ ,, ,, bb ra rn "
		"rn r5 bb ,, ,, ,, ,, ,, ,, ,, ,, .. ,, bb ,, ,, ,, ,, bb .. r2 "
		"rn rn rd ,, ,, ,, .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, bb r3 rn "
		"rn r4 bb ,, ,, ,, ,, ,, ,, bb .. ,, ,, ,, ,, .. ,, bb ,, ra rn "
		"rn rn r5 ,, ,, ,, ,, ,, ,, ,, ,, ,, .. ,, ,, ,, ,, ,, ,, bb r2 "
		"rn xx xx xx ,, ,, .. ,, ,, .. ,, ,, ,, ,, ,, ,, ,, xx xx xx rn "
		"rn rn rn xx ,, .. .. ,, ,, ,, ,, ,, ,, ,, ,, bb ,, xx rn rn rn "
		"rn rn rn xx ,, ,, ,, ,, .. ,, ,, .. ,, ,, ,, ,, ,, xx rn rn rn "
		"rn rn rn xx xx ,, bb ,, ,, ,, ,, .. ,, ,, ,, ,, xx xx rn rn rn "
		"rn rn rn rn xx xx xx xx bb ,, ,, ,, bb xx xx xx xx rn rn rn rn "
		"rn rn rn rn rn rn rn xx ,, ,, bb ,, r3 xx rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn xx bb r3 r5 bb r2 xx rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn xx r1 rn rn r1 rn xx rn rn rn rn rn rn rn "
	)
	)
	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	(list
		(put (mk-monman) 0 0)
		(put (spawn-pt 'yellow-slime) 8 9)
		(put (spawn-pt 'yellow-slime) 13 11)
	); objects
	
	 
	 (list
		'on-entry-to-dungeon-room
		'losthalls-room-handle-rroom
		'losthalls-handle-rshrine
	) ;; hooks
	nil
)

(kern-mk-place 
	'p_lost_halls_fountains
	"The Lost Halls"
	 nil          ; sprite
	(kern-mk-map nil 21 21 pal_expanded
	(list
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn xx xx xx xx xx xx xx xx xx xx xx xx xx rn rn rn rn "
		"rn rn rn xx xx ,, .. .. .. .. .. .. %f .. .. ,, xx xx rn rn rn "
		"rn rn rn xx ,, .. ,, .. .. .. .. .. .. ,, ,, .. .. xx rn rn rn "
		"rn rn rn xx .. .. ~2 ~~ ~1 ,, ,, .. _s _s _s .. bb xx rn rn rn "
		"rn xx xx xx .. .. ~2 __ ~~ .. .. .. _s _s _s .. .. xx xx xx rn "
		"r4 bb .. xx .. .. ~a ~8 ~c .. .. .. _s _s _s %% %d xx bb ra rn "
		"rn r5 bb xx .. %f .. .. .. .. %7 .. .. %% %% %c .. xx bb .. r2 "
		"rn rn r9 xx .. .. .. .. .. bb %% %% ,, .. %e .. .. xx bb r3 rn "
		"rn r4 bb xx .. .. .. %% ,, %% %c .. .. .. .. .. %7 xx ,, ra rn "
		"rn rn r1 xx ,, .. _s _s _s %% .. .. _s _s _s .. %e xx ,, bb r2 "
		"rn xx xx xx .. .. _s _s bb .. .. %% _s _s _s %% .. xx xx xx rn "
		"rn rn rn xx .. %% _s _s _s .. %b %% _s _s _s %% %5 xx rn rn rn "
		"rn rn rn xx ,, %% %% %% .. ,, .. .. %% %% .. %% %c xx rn rn rn "
		"rn rn rn xx xx .. .. .. .. .. .. .. .. .. %b %c xx xx rn rn rn "
		"rn rn rn rn xx xx xx xx xx xx xx xx xx xx xx xx xx rn rn rn rn "
		"rn rn rn rn rn rn rn xx xx xx xx ,, r2 xx rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn xx bb r2 r4 bb r2 xx rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn xx r1 rn rn r1 rn xx rn rn rn rn rn rn rn "
	)
	)
	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	(list
		(put (mk-monman) 0 0)
		(put (spawn-pt 'yellow-slime) 12 8)
		(put (spawn-pt 'yellow-slime) 12 12)
		(put (spawn-pt 'green-slime) 9 13)
		(put (spawn-pt 'green-slime) 7 8)
	); objects
	
	 
	 (list
		'on-entry-to-dungeon-room
		'losthalls-room-handle-rroom
	) ;; hooks
	nil
)

(drop-random-corpses p_lost_halls_fountains 3)

(kern-mk-place 
	'p_lost_halls_storage
	"The Lost Halls"
	 nil          ; sprite
	(kern-mk-map nil 21 21 pal_expanded
	(list
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn xx xx xx xx xx xx xx xx xx xx xx xx xx rn rn rn rn "
		"rn rn rn xx xx .. 00 .. ,, 00 xx bb .. .. .. 00 xx xx rn rn rn "
		"rn rn rn xx .. .. .. ,, .. .. rr .. 00 .. ,, .. .. xx rn rn rn "
		"rn rn rn xx .. .. .. .. 00 .. xx ,, 00 .. .. .. ,, xx rn rn rn "
		"rn xx xx xx xx d, xx rr xx xx xx xx xx rr xx d, xx xx xx xx rn "
		"r4 bb .. xx xx .. xx ,S ,T ,O ,R ,A ,G ,E xx .. xx xx bb ra rn "
		"rn r5 bb xx .. .. .. .. .. .. .. .. .. .. .. .. .. xx bb .. r2 "
		"rn rn r9 xx ,, .. .. .. .. ,, .. bb .. .. ,, .. ,, xx bb r3 rn "
		"rn r4 bb xx .. bb .. ,, .. .. .. .. .. .. .. .. .. xx ,, ra rn "
		"rn rn r1 xx xx rr d, xx xx xx d, xx xx xx d, rr xx xx ,, bb r2 "
		"rn xx xx xx xx 00 .. .. rr .. .. .. bb .. .. .. xx xx xx xx rn "
		"rn rn rn xx .. .. .. .. xx .. .. .. xx .. ,, ,, .. xx rn rn rn "
		"rn rn rn xx 00 .. ,, .. xx .. ,, .. rr .. .. ,, .. xx rn rn rn "
		"rn rn rn xx xx .. .. xx xx .. .. .. xx r5 .. 00 xx xx rn rn rn "
		"rn rn rn rn xx xx xx xx xx xx xx xx xx xx xx xx xx rn rn rn rn "
		"rn rn rn rn rn rn rn xx xx xx xx ,, r2 xx rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn xx bb r2 r4 bb r2 xx rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn xx r1 rn rn r1 rn xx rn rn rn rn rn rn rn "
	)
	)
	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	(list
		(put (mk-monman) 0 0)
		(put (spawn-pt 'rat) 7 4)
		(put (spawn-pt 'rat) 7 5)
		(put (spawn-pt 'rat) 7 6)
		(put (spawn-pt 'rat) 6 5)
		(put (spawn-pt 'rat) 13 4)
		(put (spawn-pt 'rat) 13 5)
		(put (spawn-pt 'rat) 13 5)
		(put (spawn-pt 'rat) 14 5)
		(put (spawn-pt 'rat) 5 15)
		(put (spawn-pt 'rat) 6 15)
		(put (spawn-pt 'rat) 7 15)
		(put (spawn-pt 'rat) 6 14)
		(put (spawn-pt 'rat) 14 15)
		(put (spawn-pt 'rat) 15 15)
		(put (spawn-pt 'rat) 16 15)
		(put (spawn-pt 'rat) 15 14)
	); objects
	
	 
	 (list
		'on-entry-to-dungeon-room
		'losthalls-room-handle-rroom
	) ;; hooks
	nil
)


(mk-place-music p_lost_halls_entrance 'ml-outdoor-adventure)
(mk-place-music p_lost_halls_r1 'ml-dungeon-adventure)
(mk-place-music p_lost_halls_r2 'ml-dungeon-adventure)
(mk-place-music p_lost_halls_r3 'ml-dungeon-adventure)
(mk-place-music p_lost_halls_r4 'ml-dungeon-adventure)
(mk-place-music p_lost_halls_r5 'ml-dungeon-adventure)
(mk-place-music p_lost_halls_staird 'ml-dungeon-adventure)
(mk-place-music p_lost_halls_stairu 'ml-dungeon-adventure)
(mk-place-music p_lost_halls_graves 'ml-creepy-area)
(mk-place-music p_lost_halls_rshrine 'ml-creepy-area)
(mk-place-music p_lost_halls_fountains 'ml-dungeon-adventure)
(mk-place-music p_lost_halls_storage 'ml-dungeon-adventure)


(drop-random-corpses p_lost_halls_storage 2)

;; map linkage
(prmap-linkrooms-2d 'p_lost_halls_r1 'p_lost_halls_r2 'p_lost_halls_r3 'p_lost_halls_r4 'p_lost_halls_r5)

;; 2 mapdata objects to allow different terrain types in different areas
(prmap-set-mapdata p_lost_halls_r1 (prmap-mk-mapdata "losthalls" losthalls-random-type-ns losthalls-random-type-ew losthalls-random-type-area 'losthalls-terrain-edges 'losthalls-terrain-area 'losthalls-room-blitstats 'losthalls-room-custom-hardlinks))
(prmap-set-mapdata p_lost_halls_r2 (prmap-mk-mapdata "losthalls" losthalls-random-type-ns losthalls-random-type-ew losthalls-random-type-area 'losthalls-ruin-edges 'losthalls-ruin-area 'losthalls-ruin-blitstats 'losthalls-ruin-custom-hardlinks))

;; random locations must not clash
(define (lost-halls-grave-x stair-x stair-y)
	(if (zero? 
		(cond
			((> stair-y 6) (kern-dice-roll "1d2-1"))
			((< stair-x -1) 1)
			((> stair-x 1) 0)
			(#t (kern-dice-roll "1d2-1"))
		))
		(kern-dice-roll "1d2-4")
		(kern-dice-roll "1d2+1")
	))

(let* ((losthalls-hardlinks (prmap-params-hardlinks (prmap-get-mapdata p_lost_halls_r1)))
		(stairdown-x (kern-dice-roll "1d5-3"))
		(stairdown-y (kern-dice-roll "1d2+5"))
		(grave-x (lost-halls-grave-x stairdown-x stairdown-y))
		(grave-y (kern-dice-roll "1d5"))
		)
	(define (link-rm xloc yloc zloc dir target maptemplate passable . hooks)
		(prmap-room-hardlink-set! xloc yloc zloc losthalls-hardlinks dir target maptemplate passable hooks)
		)
	
;;---------------------------------------------------------
;; hardlink setup
;; 
;;			x	y	z	dir		target				template			passable	hooks

(link-rm	0	0	0	north	nil					'm_losthalls_cpassage	#t)
(link-rm	0	1	0	south	nil					'm_losthalls_cpassage	#t)
(link-rm	0	0	0	south	'p_lost_halls_entrance		'm_losthalls_special	#t)

;; stair down in random loc
(prmap-mk-roomdata 'p_lost_halls_staird stairdown-x stairdown-y 0 (list 'p_lost_halls_r1 'p_lost_halls_r2 'p_lost_halls_r3 'p_lost_halls_r4))

(link-rm	stairdown-x	(- stairdown-y 1)	0	north	'p_lost_halls_staird	'm_losthalls_cpassage #t)
(link-rm	stairdown-x	stairdown-y	0	south	nil		'm_losthalls_cpassage	#t)
(link-rm	stairdown-x	(+ stairdown-y 1)	0	south	'p_lost_halls_staird	nil					#t)
(link-rm	(- stairdown-x 1)	stairdown-y	0	east	'p_lost_halls_staird	nil					#t)
(link-rm	(+ stairdown-x 1)	stairdown-y	0	west	'p_lost_halls_staird	nil 				#t)

(println "stair at " stairdown-x " " stairdown-y)

;; cemetary in random loc
(prmap-mk-roomdata 'p_lost_halls_graves grave-x grave-y 0 (list 'p_lost_halls_r1 'p_lost_halls_r2 'p_lost_halls_r3 'p_lost_halls_r4))

(link-rm	grave-x	(- grave-y 1)	0	north	'p_lost_halls_graves	'm_losthalls_cruin #t)
(link-rm	grave-x	grave-y			0	south	nil						'm_losthalls_cruin	#t)
(link-rm	grave-x	(+ grave-y 1)	0	south	'p_lost_halls_graves	nil					#t)
(link-rm	(- grave-x 1)	grave-y	0	east	'p_lost_halls_graves	nil					#t)
(link-rm	(+ grave-x 1)	grave-y	0	west	'p_lost_halls_graves	nil 				#t)

)

(let ((losthalls-hardlinks (prmap-params-hardlinks (prmap-get-mapdata p_lost_halls_r2)))
		(stairup-x (kern-dice-roll "1d3+1")))
	(define (link-rm xloc yloc zloc dir target maptemplate passable . hooks)
		(prmap-room-hardlink-set! xloc yloc zloc losthalls-hardlinks dir target maptemplate passable hooks)
		)

;; stair up in random loc
(prmap-mk-roomdata 'p_lost_halls_stairu stairup-x 0 1 (list 'p_lost_halls_r1 'p_lost_halls_r2 'p_lost_halls_r3 'p_lost_halls_r4))
(link-rm	stairup-x	0	1	south	nil		'm_losthalls_stairu	#f)
(link-rm	stairup-x	1	1	south	'p_lost_halls_stairu	nil		#t)
(link-rm	(- stairup-x 1)	0	1	east	'p_lost_halls_stairu	nil					#t)
(link-rm	(+ stairup-x 1)	0	1	west	'p_lost_halls_stairu	nil 				#t)

;; warritrix' resting place
(prmap-mk-roomdata 'p_lost_halls_rshrine 3 5 1 (list 'p_lost_halls_r1 'p_lost_halls_r2 'p_lost_halls_r3 'p_lost_halls_r4))
(link-rm	3	5	1	north	nil		'm_losthalls_rwall	#f)
(link-rm	3	5	1	south	nil		'm_losthalls_rrooms	#t)
(link-rm	3	4	1	north	'p_lost_halls_rshrine	'm_losthalls_rrooms		#t)
(link-rm	2	5	1	east	'p_lost_halls_rshrine	nil					#t)
(link-rm	4	5	1	west	'p_lost_halls_rshrine	nil 				#t)

;; fountains
(prmap-mk-roomdata 'p_lost_halls_fountains 5 3 1 (list 'p_lost_halls_r1 'p_lost_halls_r2 'p_lost_halls_r3 'p_lost_halls_r4))
(link-rm	5	3	1	west	nil		'm_losthalls_rhall	#t)
(link-rm	5	4	1	south	'p_lost_halls_fountains	nil		#t)
(link-rm	4	3	1	east	'p_lost_halls_fountains	'm_losthalls_rhall	#t)
(link-rm	5	2	1	north	'p_lost_halls_fountains	nil 				#t)

;; storage
(prmap-mk-roomdata 'p_lost_halls_storage 2 4 1 (list 'p_lost_halls_r1 'p_lost_halls_r2 'p_lost_halls_r3 'p_lost_halls_r4))
(link-rm	2	4	1	north	nil						'm_losthalls_rwall	#f)
(link-rm	3	4	1	west	'p_lost_halls_storage	nil	#t)
(link-rm	2	5	1	south	'p_lost_halls_storage	'm_losthalls_rwall		#t)
(link-rm	1	4	1	east	'p_lost_halls_storage	nil					#t)
(link-rm	2	3	1	north	'p_lost_halls_storage	nil 				#t)
)

;;flag for checking if cohesion check still needs to be performed
(mutable-list-set (prmap-get-mapdata p_lost_halls_r1) 10 #t)