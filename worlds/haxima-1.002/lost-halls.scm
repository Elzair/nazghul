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
	nil          ; sprite
	(kern-mk-map nil 21 21 pal_expanded
	(list
		"rr rr rr rr {8 .. bb .. {8 .. .. .. .. .. .. {8 rr rr rr rr rr "
		"rr rr rr rr {{ bb .. {c {{ {2 .. .. .. bb {c {{ rr rr rr rr rr "
		"rr rr rr rr {{ {2 {4 {{ {{ bb .. .. .. {4 {{ rr rr rr rr rr rr "
		"rr rr rr rr rr .. .. {5 {{ {2 .. .. .. {4 {{ {{ {2 rr rr rr rr "
		"rr rr rr rr rr rr .. .. {1 .. .. .. .. rr {{ {{ {a rr rr rr rr "
		"rr rr rr rr rr .. .. bb .. .. .. .. .. rr rr {{ {{ rr rr rr rr "
		"rr rr rr rr rr rr {8 .. .. .. .. .. {4 {{ {{ {7 {{ rr rr rr rr "
		"rr rr rr rr {{ {6 {{ {a .. .. .. .. .. {1 bb .. rr rr rr rr rr "
		"rr rr rr {{ {3 {4 {{ {{ {2 .. .. .. .. .. .. {4 {{ rr rr rr rr "
		"rr rr rr {1 xx xx xx {{ {2 .. .. .. .. xx xx xx {{ rr rr rr rr "
		"rr xx xx xx xx xx xx {1 .. .. .. .. .. xx xx xx xx xx xx rr rr "
		"^^ ^^ ^^ {{ xx xx xx .. .. .. .. .. .. xx xx xx {{ ^^ ^^ ^^ rr "
		"^^ ^^ ^^ {{ {a {8 .. .. .. .. .. .. .. .. {4 {{ {{ ^^ ^^ ^^ rr "
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
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
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
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
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
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
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
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
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
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
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
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
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
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
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
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
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
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
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
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
  "rr rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr rr rr rr "
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
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr xx xx xx bb xx xx rr xx xx xx rr rr rr rr rr rr "
		"rr rr rr rr rr rr .. .. .. .. .. .. .. .. rr rr rr rr rr rr rr "
		"rr rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr rr "
		"rr rr rr {{ {2 .. .. .. .. rr rr rr rr .. .. .. .. rr rr rr rr "
		"rr rr xx {1 .. .. .. rr rr rr rr rr rr rr .. .. .. .. xx rr rr "
		"rr rr xx .. .. .. rr rr xx d, d, d, xx rr rr .. .. .. bb rr rr "
		"rr rr xx bb .. .. rr rr xx ,, ,, ,, xx rr rr .. .. .. xx rr rr "
		"rr rr bb .. .. .. rr rr xx cc cc cc xx rr .. .. .. .. xx rr rr "
		"rr rr rr .. .. .. .. rr xx ,, ,, ,, xx rr .. .. .. .. xx rr rr "
		"rr rr xx bb .. .. .. rr xx cc cc cc xx xx .. .. .. .. rr rr rr "
		"rr rr xx .. .. .. .. xx xx ,, ,, ,, xx xx .. .. .. .. rr rr rr "
		"rr rr rr .. .. .. xx xx xx cc cc cc xx xx xx .. .. .. xx rr rr "
		"rr rr xx .. .. .. xx xx xx ,, ,, ,, xx xx xx .. .. {8 xx rr rr "
		"rr rr rr rr .. .. .. .. .. .. .. .. .. .. .. .. {4 {{ xx rr rr "
		"rr rr rr rr .. {8 {8 .. .. .. .. .. .. .. .. .. .. rr rr rr rr "
		"rr rr rr rr rr {{ {{ {2 .. .. .. .. .. .. .. rr rr rr rr rr rr "
		"rr rr rr rr rr rr xx xx rr rr xx rr xx xx xx xx rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
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
	 (put (mk-trap-door 'p_lost_halls_stairu 10 18) 9 7)
	 (put (mk-trap-door 'p_lost_halls_stairu 10 18) 10 7)
	 (put (mk-trap-door 'p_lost_halls_stairu 10 18) 11 7)
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
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr xx xx xx xx xx xx xx xx xx xx xx xx xx rr rr rr rr "
		"rr rr rr xx xx .. .. .. .. .. .. .. .. .. .. .. xx xx rr rr rr "
		"rr rr rr xx .. .. .. .. .. .. .. .. .. .. .. .. .. xx rr rr rr "
		"rr rr rr xx .. .. .. .. ,, .. ,, .. .. .. ,, .. .. xx rr rr rr "
		"rr rr rr xx .. .. .. .. .. .. .. .. ,, .. .. .. .. xx rr rr rr "
		"rr rr rr xx .. .. .. ,, ,, ,, ,, .. .. .. .. .. .. xx rr rr rr "
		"rr rr rr xx .. .. .. ,, .. .. .. ,, ,, ,, ,, .. .. xx rr rr rr "
		"rr rr rr xx .. .. xx ,, ,, ,, ,, ,, ,, ,, xx .. .. xx rr rr rr "
		"rr rr rr xx .. xx xx xx cc cc cc cc cc xx xx xx .. xx rr rr rr "
		"rr rr rr xx .. xx xx xx ,, ,, ,, ,, ,, xx xx xx .. xx rr rr rr "
		"rr rr rr xx .. .. xx xx cc cc cc cc cc xx xx .. .. xx rr rr rr "
		"rr rr rr xx .. .. xx xx xx ,, ,, ,, xx xx xx .. .. xx rr rr rr "
		"rr rr rr xx .. .. .. xx xx cc cc cc xx xx .. .. .. xx rr rr rr "
		"rr rr rr xx xx .. .. xx xx ,, ,, ,, xx xx .. .. xx xx rr rr rr "
		"rr rr rr rr xx xx xx xx xx cc cc cc xx xx xx xx xx rr rr rr rr "
		"rr rr rr rr rr rr rr rr xx ,, ,, ,, xx rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr xx d, d, d, xx rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr xx xx xx xx xx rr rr rr rr rr rr rr rr "
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
	 (put (mk-trap-door 'p_lost_halls_staird 10 8) 9 19)
	 (put (mk-trap-door 'p_lost_halls_staird 10 8) 10 19)
	 (put (mk-trap-door 'p_lost_halls_staird 10 8) 11 19)
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
		"rr rr rr rr rr rr rr xx rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr xx bb rr rr bb rr xx rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr xx rr bb .. .. bb xx rr rr rr rr rr rr rr "
		"rr rr rr rr xx xx xx xx .. .. .. .. .. xx xx xx xx rr rr rr rr "
		"rr rr rr xx xx .. .. .. .. .. .. .. .. .. .. .. xx xx rr rr rr "
		"rr rr rr xx .. .. .. .. .. .. .. .. .. .. .. {8 .. xx rr rr rr "
		"rr rr rr xx .. .. .. @@ ,R @@ ,I @@ ,P bb {4 {{ {2 xx rr rr rr "
		"rr xx xx xx .. .. .. .. .. .. .. .. .. .. .. {1 .. xx xx xx rr "
		"rr bb .. bb .. .. 00 .. 00 .. 00 .. 00 .. 00 .. .. .. bb rr rr "
		"rr rr bb .. .. {4 {{ {6 {{ {6 {{ {e {{ {6 {{ {2 .. .. bb .. rr "
		"rr rr rr .. .. {4 {{ {6 bb {6 {{ {{ {{ {6 {{ {2 .. .. bb rr rr "
		"rr rr bb .. .. .. {1 .. {1 .. {1 {1 {1 .. {1 .. .. bb .. rr rr "
		"rr rr rr .. .. .. 00 .. 00 .. 00 .. bb .. 00 .. .. .. .. bb rr "
		"rr xx xx xx .. {4 {{ {6 {{ {6 {{ {6 {{ {6 {{ {2 .. xx xx xx rr "
		"rr rr rr xx .. {c {{ {6 {{ {6 {{ {6 {{ {6 {{ {2 .. xx rr rr rr "
		"rr rr rr xx {4 {{ {3 .. {1 .. {1 .. {1 .. {1 .. .. xx rr rr rr "
		"rr rr rr xx xx {{ {2 .. .. .. .. .. bb .. bb .. xx xx rr rr rr "
		"rr rr rr rr xx xx xx xx bb .. .. .. bb xx xx xx xx rr rr rr rr "
		"rr rr rr rr rr rr rr xx .. .. bb .. rr xx rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr xx bb rr rr bb rr xx rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr xx rr rr rr rr rr xx rr rr rr rr rr rr rr "
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
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr xx xx xx xx xx xx xx xx xx xx xx xx xx rr rr rr rr "
		"rr rr rr xx xx @@ @@ @@ @@ ,, ++ ,, @@ @@ @@ @@ xx xx rr rr rr "
		"rr rr rr xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, bb xx rr rr rr "
		"rr rr rr xx ,, ~~ ~~ ~~ ,, ,, ,, ,, ,, ~~ ~~ ~~ ,, xx rr rr rr "
		"rr xx xx xx ,, b~ _! ~~ ,, ,, aa ,, ,, ~~ _! ~~ ,, xx xx xx rr "
		"rr bb .. bb ,, ~~ ~~ ~~ ,, ,, ,, ,, ,, ~~ ~~ ~~ ,, ,, bb rr rr "
		"rr rr bb ,, ,, ,, ,, ,, ,, ,, ,, .. ,, bb ,, ,, ,, ,, bb .. rr "
		"rr rr rr ,, ,, ,, .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, bb rr rr "
		"rr rr bb ,, ,, ,, ,, ,, ,, bb .. ,, ,, ,, ,, .. ,, bb ,, rr rr "
		"rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, .. ,, ,, ,, ,, ,, ,, bb rr "
		"rr xx xx xx ,, ,, .. ,, ,, .. ,, ,, ,, ,, ,, ,, ,, xx xx xx rr "
		"rr rr rr xx ,, .. .. ,, ,, ,, ,, ,, ,, ,, ,, bb ,, xx rr rr rr "
		"rr rr rr xx ,, ,, ,, ,, .. ,, ,, .. ,, ,, ,, ,, ,, xx rr rr rr "
		"rr rr rr xx xx ,, bb ,, ,, ,, ,, .. ,, ,, ,, ,, xx xx rr rr rr "
		"rr rr rr rr xx xx xx xx bb ,, ,, ,, bb xx xx xx xx rr rr rr rr "
		"rr rr rr rr rr rr rr xx ,, ,, bb ,, rr xx rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr xx bb rr rr bb rr xx rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr xx rr rr rr rr rr xx rr rr rr rr rr rr rr "
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
	 (put (mk-corpse2
       '(
        (1 t_rune_l_init)
        (1 t_armor_chain_4)
        (1 t_chain_coif_4)
        (1 t_sword_4)
        (1 t_shield_4)
        (1 t_warritrix_orders)
        )) 10 10)
		(put (spawn-pt 'yellow-slime) 8 9)
		(put (spawn-pt 'yellow-slime) 13 11)
	); objects
	
	 
	 (list
		'on-entry-to-dungeon-room
		'losthalls-room-handle-rroom
	) ;; hooks
	nil
)

(drop-random-corpses p_lost_halls_rshrine 5)

(kern-mk-place 
	'p_lost_halls_fountains
	"The Lost Halls"
	 nil          ; sprite
	(kern-mk-map nil 21 21 pal_expanded
	(list
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr xx xx xx xx xx xx xx xx xx xx xx xx xx rr rr rr rr "
		"rr rr rr xx xx ,, .. .. .. .. .. .. %f .. .. ,, xx xx rr rr rr "
		"rr rr rr xx ,, .. ,, .. .. .. .. .. .. ,, ,, .. .. xx rr rr rr "
		"rr rr rr xx .. .. ~~ ~~ ~~ ,, ,, .. _s _s _s .. bb xx rr rr rr "
		"rr xx xx xx .. .. ~~ __ ~~ .. .. .. _s _s _s .. .. xx xx xx rr "
		"rr bb .. xx .. .. ~~ ~~ ~~ .. .. .. _s _s _s %% %d xx bb rr rr "
		"rr rr bb xx .. %f .. .. .. .. %7 .. .. %% %% %c .. xx bb .. rr "
		"rr rr rr xx .. .. .. .. .. bb %% %% ,, .. %e .. .. xx bb rr rr "
		"rr rr bb xx .. .. .. %% ,, %% %c .. .. .. .. .. %7 xx ,, rr rr "
		"rr rr rr xx ,, .. _s _s _s %% .. .. _s _s _s .. %e xx ,, bb rr "
		"rr xx xx xx .. .. _s _s bb .. .. %% _s _s _s %% .. xx xx xx rr "
		"rr rr rr xx .. %% _s _s _s .. %b %% _s _s _s %% %5 xx rr rr rr "
		"rr rr rr xx ,, %% %% %% .. ,, .. .. %% %% .. %% %c xx rr rr rr "
		"rr rr rr xx xx .. .. .. .. .. .. .. .. .. %b %c xx xx rr rr rr "
		"rr rr rr rr xx xx xx xx xx xx xx xx xx xx xx xx xx rr rr rr rr "
		"rr rr rr rr rr rr rr xx xx xx xx ,, rr xx rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr xx bb rr rr bb rr xx rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr xx rr rr rr rr rr xx rr rr rr rr rr rr rr "
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
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr xx xx xx xx xx xx xx xx xx xx xx xx xx rr rr rr rr "
		"rr rr rr xx xx .. 00 .. ,, 00 xx bb .. .. .. 00 xx xx rr rr rr "
		"rr rr rr xx .. .. .. ,, .. .. rr .. 00 .. ,, .. .. xx rr rr rr "
		"rr rr rr xx .. .. .. .. 00 .. xx ,, 00 .. .. .. ,, xx rr rr rr "
		"rr xx xx xx xx d, xx rr xx xx xx xx xx rr xx d, xx xx xx xx rr "
		"rr bb .. xx xx .. xx ,S ,T ,O ,R ,A ,G ,E xx .. xx xx bb rr rr "
		"rr rr bb xx .. .. .. .. .. .. .. .. .. .. .. .. .. xx bb .. rr "
		"rr rr rr xx ,, .. .. .. .. ,, .. bb .. .. ,, .. ,, xx bb rr rr "
		"rr rr bb xx .. bb .. ,, .. .. .. .. .. .. .. .. .. xx ,, rr rr "
		"rr rr rr xx xx rr d, xx xx xx d, xx xx xx d, rr xx xx ,, bb rr "
		"rr xx xx xx xx 00 .. .. rr .. .. .. bb .. .. .. xx xx xx xx rr "
		"rr rr rr xx .. .. .. .. xx .. .. .. xx .. ,, ,, .. xx rr rr rr "
		"rr rr rr xx 00 .. ,, .. xx .. ,, .. rr .. .. ,, .. xx rr rr rr "
		"rr rr rr xx xx .. .. xx xx .. .. .. xx rr .. 00 xx xx rr rr rr "
		"rr rr rr rr xx xx xx xx xx xx xx xx xx xx xx xx xx rr rr rr rr "
		"rr rr rr rr rr rr rr xx xx xx xx ,, rr xx rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr xx bb rr rr bb rr xx rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr xx rr rr rr rr rr xx rr rr rr rr rr rr rr "
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

;; stair down in random loc
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