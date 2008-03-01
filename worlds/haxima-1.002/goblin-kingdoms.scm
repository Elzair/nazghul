;;;;
;;;; goblin-kingdoms.scm -- dungeon rooms for the first level of Kurpolis
;;;;

;;
;; load characters
;;
(kern-load "douglas.scm")

;;
;; define dungeon rooms
;;

(mk-dungeon-room
 'p_kurpolis_entrance "Entrance to Kurpolis"
	(list
		"rr rr rr rr xx xx x! xx xx && xx xx x! xx xx rn rn rn rn "
		"rr .. .. .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rn rn rn rn "
		"xx xx x! xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rn rn rn rn "
		"xx xx ,, ,, ,, ,, ,, [[ @@ @@ @@ ]] ,, ,, x! rn rn rn rn "
		"xx xx ,, xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rn rn rn rn "
		"xx xx ,, xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rn r8 r8 rn "
		"xx xx ,, xx xx xx xx xx xx x! xx xx xx xx xx rc bb ,, ra "
		"xx xx ,, xx xx .K .U .R .P .O .L .I .S xx ,, bb bb bb ,, "
		"xx xx ,, xx x! ,, ,, ,, ,, ,, ,, ,, ,, x! bb bb ,, bb ,, "
		"xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, bb ,, ,, ,, "
		"x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, bb ,, "
		"xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ bb bb bb bb ,, "
		"xx xx ,, xx x! ,, ,, ,, ,, ,, ,, ,, ,, x! bb bb ,, bb r3 "
		"xx xx ,, xx xx xx xx xx xx xx xx xx xx xx xx ,, bb r3 rn "
		"xx xx ,, xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx r1 r1 rn rn "
		"xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rn rn rn rn "
		"xx xx x! xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rn rn rn rn "
		"rn rn rn rn xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rn rn rn rn "
		"rn rn rn rn xx xx xx xx xx xx xx xx xx xx xx rn rn rn rn "
	)
 (put (mk-ladder-up 'p_shard 53 18) 9 10)
 (put (mk-door) 2 8)
 (put (mk-door) 2 12)
 (put (mk-door) 4 15)
 (put (mk-door) 4 3)
 (put (mk-locked-door) 4 1)
 (put (mk-windowed-door) 13 10)
 (put (mk-chest nil '((10 t_food))) 1 1)
 (put (mk-bed) 5 17)
 (put (mk-bed) 7 17)
 (put (mk-bed) 9 17)
 (put (mk-bed) 11 17)
 (put (mk-bed) 13 17)
 (put (mk-clock) 13 5)
 (put (spawn-pt 'cave-goblin-slinger) 18 7)
 (put (spawn-pt 'cave-goblin-slinger) 18 11)
 (put (spawn-pt 'cave-goblin-berserker) 15 9)
 (put (guard-pt 'crossbowman) 12 9)
 (put (guard-pt 'crossbowman) 12 11)
 (put (guard-pt 'halberdier) 10 10)
 (put (mk-douglas) 9 9)
 )

(mk-place-music p_kurpolis_entrance 'ml-castle)

(mk-dungeon-room
 'p_goblin_crossroads "Goblin Crossroads"
	(list
		"rn rn rn rn rn rn r4 {{ {{ ,, ,, {{ ra r8 r8 rn rn rn rn "
		"rn rn rn rn rn rn rn r5 {{ ,, {# {{ {{ {{ {{ ra r8 rn rn "
		"rn rn rn rn rn rn rn r4 {{ {{ ,, {5 {{ rf {{ {{ {{ r2 rn "
		"rn rn rn rn rn rn rn rn r5 {{ ,, bb {5 {{ {{ {{ {{ r2 rn "
		"rn rn rn rn rn rn rn rn r4 ,, {{ {a {8 {1 {5 {{ {{ ra rn "
		"rn rn rn rn rn rn rn rn rn r5 {{ {{ {{ {a bb {d {{ {{ r2 "
		"r8 r8 r8 rn rn rn rn rn rn r4 bb {{ {{ {{ {e {{ {{ {{ ra "
		"{{ {{ {{ ra r8 rn rn rn rn rn r5 {{ {{ {{ {{ {{ {{ {{ {{ "
		",, {{ {{ {a bb ra rn rn rn rn r4 {{ {{ {{ {{ {{ ,, {C ,, "
		",, {E ,, {{ {6 {{ ra rn rn rn rc {{ {{ ,, ,, ,, {A ,, ,, "
		",, ,, ,, {1 ,, ,, {{ ra r8 rc {{ {{ {{ {% ,, ,, ,, {% ,, "
		"{{ {{ {{ {a bb .. {5 {{ {{ {{ {{ {{ {7 {{ {{ {{ {{ {{ {{ "
		"r5 {{ {{ {{ {2 {8 bb {5 {{ {{ {{ {3 bb {d {{ {{ {{ r3 r1 "
		"rn r5 {{ r3 rd {{ {a .. {5 {{ {{ {2 {c {{ {{ {{ r3 rn rn "
		"rn rn r1 r4 {{ {{ {{ {a {8 {1 {1 {4 {{ {{ {{ {{ r2 rn rn "
		"rn rn rn r4 {{ {{ {{ {{ {{ {2 bb {4 {{ {{ rb r1 rn rn rn "
		"rn rn rn rn r5 {{ {{ {{ {{ ,, .. {c {{ {{ {{ r2 rn rn rn "
		"rn rn rn rn rn r1 r5 {{ {{ {2 {4 {{ r3 r1 r1 rn rn rn rn "
		"rn rn rn rn rn rn r4 {{ ,, .. .. r3 rn rn rn rn rn rn rn "
	)
 (put (spawn-pt 'cave-goblin-slinger) 14 11)
 (put (spawn-pt 'cave-goblin-berserker) 15 9)
 (put (spawn-pt 'cave-goblin-priest) 16 10)
 (put (spawn-pt 'forest-goblin-hunter) 15 5)
 (put (spawn-pt 'forest-goblin-hunter) 12 7)
 (put (spawn-pt 'forest-goblin-shaman) 13 5)
 (put (spawn-pt 'forest-goblin-stalker) 16 8)
 )

(mk-place-music p_goblin_crossroads 'ml-dungeon-adventure)

(mk-dungeon-room
 'p_cave_goblin_village "Cave Goblin Village"
	(list
		"rn rn rn rn rn rn r8 r8 r8 r8 r8 r8 rn rn rn rn rn rn rn "
		"rn rn rn rn rn rc {{ {{ {{ {{ {{ {{ ra rn r8 r8 r8 rn rn "
		"rn rn rn rn r4 {{ ,, ,, ,, ,, {A {{ {{ rr {{ {{ {{ ra rn "
		"rn rn rn rn r4 {{ ,, ,, && ,, ,, bb {{ rr {{ ,, {A {{ r2 "
		"rn rn rn rn r4 {{ ,, ,, ,, ,, ,, ,, {1 .. {9 ,, ,, {{ r2 "
		"rn rn rn rn r4 {{ {a {8 ,, ,, {# {{ ,, rr {{ ,, ,, {{ r2 "
		"r8 r8 r8 r8 rc {{ {{ {{ {6 {{ bb {C ,, rr {{ {{ {{ {{ r2 "
		"{{ {{ {{ {{ bb {{ bb {{ {2 {5 {C ,, ,, rr r5 {{ {{ r3 rn "
		"{{ {{ {{ {{ bb {{ {{ {3 ,, ,, ,, ,, {# rr rn r1 r1 rn rn "
		",, {A ,, {9 .. {9 {1 ,, .. ,, ,, ,, {{ {{ ra rn rn rn rn "
		",, ,, {c {{ bb {{ {a ,, ,, ,, ,, {4 {{ {{ {{ r2 rn rn rn "
		"{{ {{ {{ {{ bb {{ {{ ,, ,, ,, ,, ,, {1 {5 {{ ra r8 rn rn "
		"r1 r1 r1 r1 r5 {{ bb {% ,, ,, ,, {# ,, .. ~C ~~ ~~ ra rn "
		"rn rn rn rn r4 {{ {{ {{ {D ,, {B {{ {{ {a ~3 -- -- ~~ r2 "
		"rn rn rn rn rn r1 r5 bb bb .. bb bb rr {{ ~a -- -- ~~ r2 "
		"rn rn rn rn rn rn r4 {{ {{ {6 {{ {{ rr {{ {H ~a ~c {G r2 "
		"rn rn rn rn rn rn r4 {{ {{ {a {5 {{ rr r5 {{ {{ {{ {{ r2 "
		"rn rn rn rn rn rn r4 {{ {{ {{ {6 {{ r2 r4 {{ {{ {{ r3 rn "
		"rn rn rn rn rn rn r4 {{ {{ {3 {4 {{ r2 rn r1 r1 r1 rn rn "
	)
 (put (spawn-pt 'cave-goblin-slinger)    5  8)
 (put (spawn-pt 'cave-goblin-slinger)    8  2)
 (put (spawn-pt 'cave-goblin-slinger)    9  3)
 (put (spawn-pt 'cave-goblin-slinger)   10 13)
 (put (spawn-pt 'cave-goblin-berserker)  6 10)
 (put (spawn-pt 'cave-goblin-berserker)  8  4)
 (put (spawn-pt 'cave-goblin-berserker)  7  3)
 (put (spawn-pt 'cave-goblin-berserker)  8 13)
 (put (spawn-pt 'cave-goblin-priest)    15  4)
 (put (spawn-pt 'cave-goblin-priest)     9  9)
 (put (guard-pt 'cave-goblin-berserker) 14  4)
 (put (guard-pt 'cave-goblin-priest)    16  4)
 (put (mk-locked-door-in-rock) 13 4)
 (put (mk-treasure-chest) 16 4)
 (put (mk-treasure-chest) 16 5)
 (put (mk-treasure-chest) 15 3)
 )

(mk-place-music p_cave_goblin_village 'ml-dungeon-adventure)

(mk-dungeon-room
 'p_trolls_den "Troll's Den"
	(list
		"rn r8 r8 r8 rn rn r4 {{ {2 ,, ,, {{ r2 rn rn rn rn rn rn "
		"r4 {{ {{ {{ ra rn r4 {{ {2 ,, {B {{ r2 rn rn rn rn rn rn "
		"r4 {{ {{ {{ {{ ra r4 {{ {a .. ,, {A r2 rn rn rn rn rn rn "
		"r4 {{ {{ {{ {{ {{ ra r5 {{ {2 ,, bb r2 rn rn rn rn rn rn "
		"rn r5 {{ {{ r7 {{ {{ re {{ ,, {c {{ r2 rn rn rn rn rn rn "
		"rn r4 {{ rb rn rd {{ {{ {{ {6 {{ {{ r2 r8 r8 rn rn rn rn "
		"rn rc {{ {{ re {{ {{ r7 {{ {a {5 {{ re {{ {{ ra r8 rn rn "
		"r4 {{ {{ {{ {6 {{ rb rn rd {{ {2 {1 {8 {5 {{ {{ {{ ra rn "
		"r4 {{ {{ {b bb {d {{ re {{ {3 .. r7 {{ {a {5 {{ {3 .. r2 "
		"r4 {{ {{ {{ {e {{ {{ {{ {{ {2 rb rn rd {{ {2 {1 .. .. r2 "
		"rn r5 {{ {{ {{ {{ r7 {{ {3 {c {{ re {{ {{ {2 .. .. && r2 "
		"rn rn r5 {{ {{ rb rn rd {4 {{ {{ {{ {3 {1 .. {8 .. .. r2 "
		"rn rn rc {{ {7 {{ re {{ {2 r3 r5 {1 bb {8 {c {{ {a .. r2 "
		"rn rc {{ bb .. {5 {{ {3 {8 ra rn r5 {4 {{ {{ {{ {{ r3 rn "
		"r4 {{ {3 .. .. .. {9 {c {{ {{ r2 rn r1 r5 {{ {{ {{ r2 rn "
		"r4 {{ {a .. .. bb {{ {{ {{ {{ r2 rn rn rn r1 r1 r1 rn rn "
		"r4 {{ {{ {a {8 bb {{ {{ r3 r1 rn rn rn rn rn rn rn rn rn "
		"rn r5 {{ {{ {{ r3 r1 r1 rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn r1 r1 r1 rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
	)
 (put (mk-ladder-up 'p_old_mine 17 17) 3 15)
 (put (spawn-pt 'troll) 16 10)
 (put (spawn-pt 'troll-geomancer) 17 9)
 (put (spawn-pt 'troll) 17 11)
 (put (kern-mk-obj t_food 1) 17 8)
 (put (kern-mk-obj t_beer 1) 16 8)
 (put (kern-mk-obj t_food 1) 16 12)
 (put (kern-mk-obj t_beer 1) 15 11)
 (put (mk-corpse-with-loot) 12 13)
 (put (mk-corpse-with-loot) 14 14)
 (put (mk-corpse-with-loot) 14 6)
 )

(mk-place-music p_trolls_den 'ml-dungeon-adventure)

(mk-dungeon-room
 'p_shamans_grove "Shaman's Grove"
	(list
		"rn rn rn rn rn rn rn rn r8 r8 r8 rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rc t7 bb t7 ra rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rc tb tt t| t| td ra rn rn rn rn rn rn "
		"rn rn rn rn rn r4 t7 bb te t& te bb t7 r2 rn rn rn rn rn "
		"rn rn rn rn rn r4 tt t5 tB aa tD t3 tt r2 rn rn rn rn rn "
		"rn rn rn rn rn r4 ta tt td tE tb t| tc r2 rn rn rn rn rn "
		"rn rn rn rn rn rn r5 te bb t7 bb te r3 rn rn rn rn rn rn "
		"rn rn rn r8 r8 r8 rn r5 t3 tt t5 r3 rn r8 r8 r8 rn rn rn "
		"rn rn rc t3 tt t5 ra r4 || |X || r2 rc |# |X |% ra rn rn "
		"rn rc t3 tc t& ta t5 re |t || || re |# |X || || |% ra rn "
		"r4 t3 tc t# .. t% te bb |X |X |t || || |X || || || |% r2 "
		"r4 tt tB .. && .. tD tb || |t || || || |X tt || || || r2 "
		"r4 ta t5 tA .. tC t7 bb || || || |t |X |t |X |X || |C r2 "
		"rn r5 ta t5 tE t3 tc r7 |X || || r7 |A |X || || |C r3 rn "
		"rn rn r5 ta tt tc r3 r4 |X || |X r2 r5 |A |t |C r3 rn rn "
		"rn rn rn r1 r1 r1 rn r4 || tt || r2 rn r1 r1 r1 rn rn rn "
		"rn rn rn rn rn rn rn r4 t| tt tt r2 rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn r4 ta tt tc r2 rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn r4 .. .. .. r2 rn rn rn rn rn rn rn "
	)
 (put (mk-ladder-down 'p_dank_cave 9 1) 14 11)
 (put (spawn-pt 'forest-goblin-shaman) 9 3)
 (put (spawn-pt 'forest-goblin-hunter) 3 11)
 (put (spawn-pt 'forest-goblin-hunter) 4 10)
 (put (spawn-pt 'forest-goblin-stalker) 5 11)
 (put (mk-chest 'spike-trap
                '((5 t_food)
                  (20 t_arrow)
                  (30 t_gold_coins)
                  (3 t_heal_potion)
                  ))
      2 13)
 (put (mk-chest 'lightning-trap
                '((2 mandrake)
                  (6 sulphorous_ash)
                  (4 blood_moss)
                  (5 garlic)
                  (5 ginseng)
                  (2 t_mana_potion)
                  (1 t_xp_potion)
                  ))
      10 1)
 )

(mk-place-music p_shamans_grove 'ml-dungeon-adventure)

(mk-dungeon-room
 'p_watchpoint  "Watchpoint"
	(list
		"rn rn rn rn rn rn r4 {{ {C ,, ,, {{ r2 rn rn rn rn rn rn "
		"rn rn rn rn rn rn rc {{ ,, ,, {4 {{ ra rn rn rn rn rn rn "
		"rn rn rn rn rn r4 bb {{ ,, .. ,, {5 {{ r2 rn rn rn rn rn "
		"rn rn rn rn rn rc {{ {{ ,, .. ,, bb {{ r2 rn rn rn r8 rn "
		"rn rn rn rn r4 bb {{ {{ {2 ,, ,, {c {{ ra rn rn rc __ r2 "
		"rn rn rn rn rc {{ {{ {{ ,, .. ,, {{ {{ {{ ra rc -- __ r2 "
		"rn rn rn r4 bb {A {{ {C oo ee oo {{ ~~ -- -- -- -- __ r2 "
		"rn rn r8 rc bb bb ~3 ~9 ~~ ee ee ~9 ~~ _! -- -- r3 r1 rn "
		"rn rc -- -- -- ~~ ~c {& oo ee ~c {{ ~~ -- rb r1 rn rn rn "
		"r4 __ _! _! -- -c {G {{ {{ ,, {{ {{ {{ {{ {{ r2 rn rn rn "
		"r4 __ -- -- -- {G {{ bb ,, ,, ,, bb {{ {{ {{ r2 rn rn rn "
		"r4 __ r3 r5 {{ {{ x. x. w+ d, w+ x. x. {{ {{ r2 rn rn rn "
		"rn r1 rn r4 {{ {{ x. .. ,, ,, ,, ,, r6 {d {{ r2 rn rn rn "
		"rn rn rn rn rd {{ x. ,, ,, ,, ,, ,, x. {{ {{ r2 rn rn rn "
		"rn rn rn r4 {{ {{ x. ,, ,, ,, ,, ,, x. {{ {{ r2 rn rn rn "
		"rn rn rn r4 {{ {{ bb ,, ,, ,, ,, .. x. {{ r3 rn rn rn rn "
		"rn rn rn rn r5 {{ xx ,, ,, ,, .. bb x. r1 rn rn rn rn rn "
		"rn rn rn rn rn r1 rn xx xx && xx xx xx rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn r1 rn rn rn rn rn rn rn rn rn "
	)
 (put (mk-ladder-down 'p_paladins_hold 3 9) 9 14)
 (put (guard-pt 'halberdier) 8 12)
 (put (guard-pt 'crossbowman) 10 12)
 )

(mk-place-music p_watchpoint 'ml-dungeon-town)

;;
;; assemble the rooms into a dungeon level
;;

(mk-dungeon-level 
 (list nil                 p_shamans_grove     nil                  )
 (list p_kurpolis_entrance p_goblin_crossroads p_cave_goblin_village)
 (list nil                 p_watchpoint        p_trolls_den         )
 )
