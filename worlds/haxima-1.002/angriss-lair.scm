;; ----------------------------------------------------------------------------
;; Map
;; ----------------------------------------------------------------------------
(kern-mk-map
 'm_angriss_lair 19 19 pal_expanded
	(list
		"|| || || || || || || || tt tt tt || || || tt || || || || "
		"|| || || || || || || || || tt || || || tt tt tt || || || "
		"|| || || || || || bb bb bb bb || || || bb .. bb || || || "
		"|| || || || bb bb bb t3 t5 bb bb || bb .. .. .. bb || || "
		"|| || || bb .. .. t3 || || t5 bb .. .. .. .. .. bb || || "
		"|| || || bb .. .. ta || || tc .. .. .. .. .. bb || || || "
		"|| || || bb bb bb .. .. .. .. .. .. .. bb bb || || || || "
		"|| || || || bb bb .. bb .. .. tC t3 tt t5 bb || || || || "
		"tt || || || bb || || || bb .. tb || || tt td bb || || tt "
		"tt tt || || bb || tF || bb .. t% |A || tc t# bb || tt tt "
		"tt || || || bb || || || bb bb .. .. .. .. bb || || || tt "
		"|| || || || bb bb bb bb .. .. .. bb .. bb bb bb || || || "
		"|| || || bb .. tC t3 t5 tA .. .. bb .. || || bb || || || "
		"|| || || bb .. t3 || || td .. bb .. .. t% || bb || || || "
		"|| || || bb .. ta || |C t# .. bb .. .. .. .. bb || || || "
		"|| || || || bb .. .. .. .. bb || bb bb .. bb || || || || "
		"|| || || || || bb .. bb bb || || || tt tt tt || || || || "
		"|| || || || || tt tt tt || tt || || || tt || || || || || "
		"|| || || || || || tt || tt tt tt || || || || || || || || "
	)
	

 )
;;----------------------------------------------------------------------------
;; Place
;;----------------------------------------------------------------------------
(kern-mk-place 
 'p_angriss_lair ; tag
 "Entrance to Angriss's Lair"   ; name
 s_spider_web     ; sprite
 m_angriss_lair  ; map
 #f               ; wraps
 #f                     ; underground
 #f                     ; large-scale (wilderness)
 #f                     ; tmp combat place
 nil                    ; subplaces
 
 ;; neighbors
 (list
  )
 
 ;; objects
 (list
  (put (mk-monman) 0 0)
  (put (mk-ladder-down 'p_spider_cave 6 9) 6 9)
  (put (mk-ladder-down 'p_spider_cave 15 4) 15 4)
  
  (put (spawn-pt 'giant-spider faction-spider) 6 9)
  (put (spawn-pt 'giant-spider faction-spider) 15 4)
  (put (spawn-pt 'giant-spider faction-spider) 7 4)
  (put (spawn-pt 'giant-spider faction-spider) 6 13)
  (put (spawn-pt 'giant-spider faction-spider) 14 12)

  )

 (list 'on-entry-to-dungeon-room
 	'quest-rune-f-lair
 	) ; hooks
 nil ; edge entrances
 )

(mk-place-music p_angriss_lair 'ml-outdoor-adventure)

(kern-load "angriss.scm")

(mk-dungeon-room
 'p_angriss_throne_room  "Angriss's Throne Room"
 (list
		"rn rn rn rn r8 r8 r8 r8 r8 r8 r8 r8 r8 r8 rn rn rn rn rn "
		"rn rn r8 r4 .. .. .. .. .. .. .. .. .. .. r2 rn r8 rn rn "
		"rn rc .. re .. .. bb .. .. .. .. .. .. .. ra r4 .. ra rn "
		"r4 .. .. .. .. .. bb bb .. bb bb bb .. .. .. re .. .. r2 "
		"r4 .. .. .. .. bb .. bb bb bb .. bb r7 .. .. .. .. .. r2 "
		"r4 .. .. .. bb bb .. .. .. .. bb rb rc bb .. .. .. .. r2 "
		"rn r5 .. .. bb .. .. bb bb .. .. bb bb .. bb .. .. .. r2 "
		"rn rc .. bb .. .. bb .. .. .. .. .. bb .. bb bb .. .. r2 "
		"r4 .. .. bb .. bb .. .. .. .. .. bb .. .. bb .. .. rb rn "
		"r4 .. .. bb .. .. bb .. .. .. .. .. bb .. bb .. .. .. r2 "
		"r4 .. .. bb bb .. bb .. .. .. .. .. bb bb .. bb .. .. r2 "
		"r4 .. .. .. .. .. bb bb .. .. .. bb bb .. .. bb .. .. r2 "
		"r4 .. .. .. bb rf bb .. bb .. bb .. .. bb bb .. .. .. r2 "
		"r4 .. .. .. bb bb bb bb bb bb bb .. bb bb bb .. .. .. r2 "
		"r4 bb .. .. .. bb .. rf bb .. bb .. .. bb .. .. .. .. r2 "
		"r4 bb bb .. .. .. bb bb bb bb .. bb .. .. .. .. .. .. r2 "
		"rn rd .. .. .. .. .. .. .. .. .. .. .. .. r7 .. .. bb r2 "
		"r4 bb bb .. bb .. .. .. .. .. .. .. .. r3 r4 .. bb bb r2 "
		"rn r1 r1 r1 r1 r1 r5 .. .. .. .. .. r3 rn rn r1 r1 r1 rn "
  )
 
 (put (mk-angriss) 9 9)

 (put (kern-mk-obj F_web_perm 1) 9  6)
 (put (kern-mk-obj F_web_perm 1) 10 6)
 
 (put (kern-mk-obj F_web_perm 1) 7  7)
 (put (kern-mk-obj F_web_perm 1) 8  7)
 (put (kern-mk-obj F_web_perm 1) 9  7)
 (put (kern-mk-obj F_web_perm 1) 10 7)
 (put (kern-mk-obj F_web_perm 1) 11 7)
 
 (put (kern-mk-obj F_web_perm 1) 6  8)
 (put (kern-mk-obj F_web_perm 1) 7  8)
 (put (kern-mk-obj F_web_perm 1) 8  8)
 (put (kern-mk-obj F_web_perm 1) 9  8)
 (put (kern-mk-obj F_web_perm 1) 10 8)
 
 (put (kern-mk-obj F_web_perm 1) 7  9)
 (put (kern-mk-obj F_web_perm 1) 8  9)
 (put (kern-mk-obj F_web_perm 1) 9  9)
 (put (kern-mk-obj F_web_perm 1) 10 9)
 (put (kern-mk-obj F_web_perm 1) 11 9)
 
 (put (kern-mk-obj F_web_perm 1) 7  10)
 (put (kern-mk-obj F_web_perm 1) 8  10)
 (put (kern-mk-obj F_web_perm 1) 9  10)
 (put (kern-mk-obj F_web_perm 1) 10 10)
 (put (kern-mk-obj F_web_perm 1) 11 10)

 (put (kern-mk-obj F_web_perm 1) 8  11)
 (put (kern-mk-obj F_web_perm 1) 9  11)
 (put (kern-mk-obj F_web_perm 1) 10 11)

 (put (kern-mk-obj F_web_perm 1) 9  12)

 (put (kern-mk-obj F_web_perm 1) 11 12)
 (put (kern-mk-obj F_web_perm 1) 11 13)
 (put (kern-mk-obj F_web_perm 1) 11 14)
 (put (kern-mk-obj F_web_perm 1) 12 14)
 (put (kern-mk-obj F_web_perm 1) 5 6)
 
 )

(mk-place-music p_angriss_throne_room 'ml-dungeon-adventure)

;; corpse & treasure heaps
(put-random-stuff p_angriss_throne_room
                  (mk-rect 0 0 19 19)
                  (lambda (loc)
                    (eqv? (kern-place-get-terrain loc)
                          t_grass))
                  (lambda (loc)
                    (kern-obj-put-at (mk-corpse-with-loot)
                                     loc))
                  50)

;; spider eggs
(put-random-stuff p_angriss_throne_room
                  (mk-rect 6 6 7 7)
                  (lambda (loc) #t)
                  (lambda (loc)
                    (kern-obj-put-at (mk-spider-egg) loc))
                  20)



(mk-dungeon-room
 'p_spider_cave "Spider Cave"
	(list
		"rn r8 r8 r8 r8 r8 rc .. .. .. .. .. r2 rn r8 r8 r8 r8 rn "
		"r4 bb .. .. .. .. .. .. .. .. .. .. r2 rc bb .. bb bb r2 "
		"rn r5 .. .. .. .. .. .. .. .. .. r3 rc bb .. .. .. bb r2 "
		"rn rn r5 .. r7 .. .. .. tC t7 tA r6 .. .. .. .! .. bb r2 "
		"rn rn rn r9 rn rd .. tC t3 tt t5 r6 .. .. .! .! .! .. r2 "
		"rn rn rc bb re t3 tt tt tt tt tc r6 bb .. .. .! .. .. r2 "
		"rn rc .. tC t3 tt tt tt tt tt t# ra r5 bb bb .. .. bb r2 "
		"r4 .. tC t3 tt tt tt tt tt tt tA .. ra r9 r9 r9 r9 r9 rn "
		"r4 .. t3 tt tt .! .! .! tt tt t5 tA .. .. bb .. .. .. r2 "
		"r4 .. tt tt tt .! .! .! tt tt tt td .. .. .. .. .. rb rn "
		"r4 .. tt tt tt .! .! .! tt tt tc t# .. .. bb .. .. .. r2 "
		"r4 .. ta tt tt tt tt tt tt tc r7 .. .. .. .. bb .. .. r2 "
		"r4 .. t% ta tt tc bb ta tc r3 r4 bb .. r3 rd .. .. rb rn "
		"r4 .. .. t% te rb r1 r1 r9 r8 r8 r9 r9 rc bb rf .. .. r2 "
		"r4 bb .. rf .. .. ra rc .. .. bb .. .. bb .. .. .. .. r2 "
		"rn r5 bb .. .. .. .. .. .. .. .. bb .. .. .. .. .. .. r2 "
		"rn rc .. .. .. bb .. .. .. .. bb .. .. .. .. bb .. bb r2 "
		"r4 .. .. .. .. .. .. .. r3 r1 r1 r1 r5 .. .. .. .. r3 rn "
		"rn r1 r1 r1 r1 r1 r1 r1 rn rn rn rn rn r1 r1 r1 r1 rn rn "
	)
	
 (put (mk-ladder-up 'p_angriss_lair 6 9) 6 9)
 (put (mk-ladder-up 'p_angriss_lair 15 4) 15 4)

 (put (spawn-pt 'queen-spider faction-spider) 9 0)
 (put (spawn-pt 'queen-spider faction-spider) 15 15)
 (put (spawn-pt 'queen-spider faction-spider) 1 17)

 ;; meat locker
 (put (mk-corpse) 15 1)
 (put (mk-corpse) 12 3)
 (put (mk-corpse) 12 4)
 (put (mk-corpse) 16 6)
 (put (mk-corpse) 17 4)
 (put (mk-spider-egg) 15 1)
 (put (mk-spider-egg) 12 3)
 (put (mk-spider-egg) 12 4)
 (put (mk-spider-egg) 16 6)
 (put (mk-spider-egg) 17 4)
 (put (kern-mk-obj web-type 1) 15 1)
 (put (kern-mk-obj web-type 1) 12 3)
 (put (kern-mk-obj web-type 1) 12 4)
 (put (kern-mk-obj web-type 1) 16 6)
 (put (kern-mk-obj web-type 1) 17 4)
 (put (kern-mk-obj web-type 1) 17 5)
 (put (kern-mk-obj web-type 1) 15 6)
 (put (kern-mk-obj web-type 1) 13 5)
 (put (kern-mk-obj t_spell_book_white_magick_2 1) 15 1)
 (put (kern-mk-obj t_gold_coins 52) 14 2)
 (put (kern-mk-obj t_gold_coins 34) 17 5)
 (put (kern-mk-obj t_bow 1) 15 6)
 (put (kern-mk-obj t_arrow 34) 13 5)
 (put (kern-mk-obj t_leather_helm 1) 16 2)
 (put (kern-mk-obj t_halberd 1) 12 3)
 (put (kern-mk-obj t_heal_potion 3) 15 1)
 (put (kern-mk-obj t_mana_potion 5) 16 6)
 (put (kern-mk-obj t_spell_book_force_magick_battle 1) 17 6)
)

(mk-place-music p_spider_cave 'ml-dungeon-adventure)

(mk-dungeon-level 
 (list p_angriss_throne_room)
 (list p_spider_cave)
 )
