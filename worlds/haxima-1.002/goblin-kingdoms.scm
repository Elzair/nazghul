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
      "rr rr rr rr xx xx x! xx xx && xx xx x! xx xx rr rr rr rr "
      "rr .. .. .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx xx x! xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx xx ,, ,, ,, ,, ,, [[ @@ @@ @@ ]] ,, ,, x! rr rr rr rr "
      "xx xx ,, xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx xx ,, xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx xx ,, xx xx xx xx xx xx x! xx xx xx xx xx rr bb ,, rr "
      "xx xx ,, xx xx .K .U .R .P .O .L .I .S xx ,, bb bb bb ,, "
      "xx xx ,, xx x! ,, ,, ,, ,, ,, ,, ,, ,, x! bb bb ,, bb ,, "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, bb ,, ,, ,, "
      "x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, bb ,, "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ bb bb bb bb ,, "
      "xx xx ,, xx x! ,, ,, ,, ,, ,, ,, ,, ,, x! bb bb ,, bb rr "
      "xx xx ,, xx xx xx xx xx xx xx xx xx xx xx xx ,, bb rr rr "
      "xx xx ,, xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx xx x! xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "rr rr rr rr xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "rr rr rr rr xx xx xx xx xx xx xx xx xx xx xx rr rr rr rr "
  ) 
 (put (mk-ladder-up 'p_shard 39 75) 9 10)
 (put (mk-door) 2 8)
 (put (mk-door) 2 12)
 (put (mk-door) 4 15)
 (put (mk-door) 4 3)
 (put (mk-locked-door) 4 1)
 (put (mk-windowed-door) 13 10)
 (put (mk-chest nil (list (list 10 t_food))) 1 1)
 (put (mk-bed) 5 17)
 (put (mk-bed) 7 17)
 (put (mk-bed) 9 17)
 (put (mk-bed) 11 17)
 (put (mk-bed) 13 17)
 (put (spawn-pt 'cave-goblin-slinger) 18 7)
 (put (spawn-pt 'cave-goblin-slinger) 18 11)
 (put (spawn-pt 'cave-goblin-berserker) 15 9)
 (put (guard-pt 'crossbowman) 12 9)
 (put (guard-pt 'crossbowman) 12 11)
 (put (guard-pt 'halberdier) 10 10)
 (put (mk-douglas) 9 9)
 )

(mk-dungeon-room
 'p_goblin_crossroads "Goblin Crossroads"
 (list
  "rr rr rr rr rr rr rr {{ {{ ,, ,, {{ rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr {{ ,, {{ {{ {{ {{ {{ rr rr rr rr "
  "rr rr rr rr rr rr rr rr {{ {{ ,, .. {{ rr {{ {{ {{ rr rr "
  "rr rr rr rr rr rr rr rr rr {{ ,, bb .. {{ {{ {{ {{ rr rr "
  "rr rr rr rr rr rr rr rr rr ,, {{ .. .. .. .. {{ {{ rr rr "
  "rr rr rr rr rr rr rr rr rr rr {{ {{ {{ .. bb .. {{ {{ rr "
  "rr rr rr rr rr rr rr rr rr rr bb {{ {{ {{ .. {{ {{ {{ rr "
  "{{ {{ {{ rr rr rr rr rr rr rr rr {{ {{ {{ {{ {{ {{ {{ {{ "
  ",, {{ {{ .. bb rr rr rr rr rr rr {{ {{ {{ {{ {{ ,, {{ ,, "
  ",, {{ ,, {{ .. {{ rr rr rr rr rr {{ {{ ,, ,, ,, {{ ,, ,, "
  ",, ,, ,, .. ,, ,, {{ rr rr rr {{ {{ {{ {{ ,, ,, ,, {{ ,, "
  "{{ {{ {{ .. bb .. .. {{ {{ {{ {{ {{ .. {{ {{ {{ {{ {{ {{ "
  "rr {{ {{ {{ .. .. bb .. {{ {{ {{ .. bb .. {{ {{ {{ rr rr "
  "rr rr {{ rr rr {{ .. .. .. {{ {{ .. .. {{ {{ {{ rr rr rr "
  "rr rr rr rr {{ {{ {{ .. .. .. .. .. {{ {{ {{ {{ rr rr rr "
  "rr rr rr rr {{ {{ {{ {{ {{ .. bb .. {{ {{ rr rr rr rr rr "
  "rr rr rr rr rr {{ {{ {{ {{ ,, .. .. {{ {{ {{ rr rr rr rr "
  "rr rr rr rr rr rr rr {{ {{ .. .. {{ rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr {{ ,, .. .. rr rr rr rr rr rr rr rr "
  )
 (put (spawn-pt 'cave-goblin-slinger) 14 11)
 (put (spawn-pt 'cave-goblin-berserker) 15 9)
 (put (spawn-pt 'forest-goblin-hunter) 15 5)
 (put (spawn-pt 'forest-goblin-hunter) 12 7)
 )

(mk-dungeon-room
 'p_cave_goblin_village "Cave Goblin Village"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr {{ {{ {{ {{ {{ {{ rr rr rr rr rr rr rr "
  "rr rr rr rr rr {{ ,, ,, ,, ,, {{ {{ {{ rr {{ {{ {{ rr rr "
  "rr rr rr rr rr {{ ,, ,, && ,, ,, bb {{ rr {{ ,, {{ {{ rr "
  "rr rr rr rr rr {{ ,, ,, ,, ,, ,, ,, .. .. .. ,, ,, {{ rr "
  "rr rr rr rr rr {{ .. .. ,, ,, {{ {{ ,, rr {{ ,, ,, {{ rr "
  "rr rr rr rr rr {{ {{ {{ .. {{ bb {{ ,, rr {{ {{ {{ {{ rr "
  "{{ {{ {{ {{ bb {{ bb {{ .. .. {{ ,, ,, rr rr {{ {{ rr rr "
  "{{ {{ {{ {{ bb {{ {{ .. ,, ,, ,, ,, {{ rr rr rr rr rr rr "
  ",, {{ ,, .. .. .. .. ,, .. ,, ,, ,, {{ {{ rr rr rr rr rr "
  ",, ,, .. {{ bb {{ .. ,, ,, ,, ,, .. {{ {{ {{ rr rr rr rr "
  "{{ {{ {{ {{ bb {{ {{ ,, ,, ,, ,, ,, .. .. {{ rr rr rr rr "
  "rr rr rr rr rr {{ bb {{ ,, ,, ,, {{ ,, .. .. ~~ ~~ rr rr "
  "rr rr rr rr rr {{ {{ {{ {{ ,, {{ {{ {{ .. ~~ -- -- ~~ rr "
  "rr rr rr rr rr rr rr bb bb .. bb bb rr {{ ~~ -- -- ~~ rr "
  "rr rr rr rr rr rr rr {{ {{ .. {{ {{ rr {{ {{ ~~ ~~ {{ rr "
  "rr rr rr rr rr rr rr {{ {{ .. .. {{ rr rr {{ {{ {{ {{ rr "
  "rr rr rr rr rr rr rr {{ {{ {{ .. {{ rr rr {{ {{ {{ rr rr "
  "rr rr rr rr rr rr rr {{ {{ .. .. {{ rr rr rr rr rr rr rr "
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
 (put (mk-locked-door) 13 4)
 (put (mk-treasure-chest) 16 4)
 (put (mk-treasure-chest) 16 5)
 (put (mk-treasure-chest) 15 3)
 )

(mk-dungeon-room
 'p_trolls_den "Troll's Den"
 (list
  "rr rr rr rr rr rr rr {{ .. ,, ,, {{ rr rr rr rr rr rr rr "
  "rr {{ {{ {{ rr rr rr {{ .. ,, {{ {{ rr rr rr rr rr rr rr "
  "rr {{ {{ {{ {{ rr rr {{ .. .. ,, {{ rr rr rr rr rr rr rr "
  "rr {{ {{ {{ {{ {{ rr rr {{ .. ,, bb rr rr rr rr rr rr rr "
  "rr rr {{ {{ rr {{ {{ rr {{ ,, .. {{ rr rr rr rr rr rr rr "
  "rr rr {{ rr rr rr {{ {{ {{ .. {{ {{ rr rr rr rr rr rr rr "
  "rr rr {{ {{ rr {{ {{ rr {{ .. .. {{ rr {{ {{ rr rr rr rr "
  "rr {{ {{ {{ .. {{ rr rr rr {{ .. .. .. .. {{ {{ {{ rr rr "
  "rr {{ {{ .. bb .. {{ rr {{ .. .. rr {{ .. .. {{ .. .. rr "
  "rr {{ {{ {{ .. {{ {{ {{ {{ .. rr rr rr {{ .. .. .. .. rr "
  "rr rr {{ {{ {{ {{ rr {{ .. .. {{ rr {{ {{ .. .. .. && rr "
  "rr rr rr {{ {{ rr rr rr .. {{ {{ {{ .. .. .. .. .. .. rr "
  "rr rr rr {{ .. {{ rr {{ .. rr rr .. bb .. .. {{ .. .. rr "
  "rr rr {{ bb .. .. {{ .. .. rr rr rr .. {{ {{ {{ {{ rr rr "
  "rr {{ .. .. .. .. .. .. {{ {{ rr rr rr rr {{ {{ {{ rr rr "
  "rr {{ .. .. .. bb {{ {{ {{ {{ rr rr rr rr rr rr rr rr rr "
  "rr {{ {{ .. .. bb {{ {{ rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr {{ {{ {{ rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 (put (mk-ladder-up 'p_old_mine 17 17) 3 15)
 (put (spawn-pt 'troll) 16 10)
 (put (spawn-pt 'troll-geomancer) 17 9)
 (put (spawn-pt 'troll) 17 11)
 (put (kern-mk-obj t_food 1) 17 8)
 (put (kern-mk-obj t_beer 1) 16 8)
 (put (kern-mk-obj t_food 1) 16 12)
 (put (kern-mk-obj t_beer 1) 15 11)
 )

(mk-dungeon-room
 'p_shamans_grove "Shaman's Grove"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr tt bb tt rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr tt tt tt tt tt rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr tt bb tt .. tt bb tt rr rr rr rr rr rr "
  "rr rr rr rr rr rr tt tt .. aa .. tt tt rr rr rr rr rr rr "
  "rr rr rr rr rr rr tt tt tt .. tt tt tt rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr tt bb tt bb tt rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr tt tt tt rr rr rr rr rr rr rr rr "
  "rr rr rr tt tt tt rr rr || || || rr rr || || || rr rr rr "
  "rr rr tt tt .. tt tt rr || || || rr || || || || || rr rr "
  "rr tt tt .. .. .. tt bb || || || || || || || || || || rr "
  "rr tt .. .. && .. .. tt || || || || || || tt || || || rr "
  "rr tt tt .. .. .. tt bb || || || || || || || || || || rr "
  "rr rr tt tt .. tt tt rr || || || rr || || || || || rr rr "
  "rr rr rr tt tt tt rr rr || || || rr rr || || || rr rr rr "
  "rr rr rr rr rr rr rr rr || tt || rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr tt tt tt rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr tt tt tt rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr "
  )
 (put (mk-ladder-down 'p_dank_cave 9 1) 14 11)
 (put (spawn-pt 'forest-goblin-shaman) 9 3)
 (put (spawn-pt 'forest-goblin-hunter) 3 11)
 (put (spawn-pt 'forest-goblin-hunter) 4 10)
 (put (spawn-pt 'forest-goblin-stalker) 5 11)
 )

(mk-dungeon-room
 'p_watchpoint  "Watchpoint"
 (list
  "rr rr rr rr rr rr rr {{ {{ ,, ,, {{ rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr {{ ,, ,, .. {{ rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr bb {{ ,, .. ,, .. {{ rr rr rr rr rr rr "
  "rr rr rr rr rr rr {{ {{ ,, .. ,, bb {{ rr rr rr rr rr rr "
  "rr rr rr rr rr bb {{ {{ .. ,, ,, .. {{ rr rr rr rr rr rr "
  "rr rr rr rr rr {{ {{ {{ ,, .. ,, {{ {{ {{ rr rr rr rr rr "
  "rr rr rr rr bb {{ {{ {{ oo ee oo {{ ~~ -- rr rr rr rr rr "
  "rr rr rr rr bb bb ~~ ~~ ~~ ee ee ~~ ~~ _! rr rr rr rr rr "
  "rr rr rr -- -- ~~ ~~ {{ oo ee ~~ {{ ~~ -- rr rr rr rr rr "
  "rr rr rr _! -- -- {{ {{ {{ ,, {{ {{ {{ {{ {{ rr rr rr rr "
  "rr rr rr -- -- {{ {{ bb ,, ,, ,, bb {{ {{ {{ rr rr rr rr "
  "rr rr rr rr {{ {{ x. x. w+ d, w+ x. x. {{ {{ rr rr rr rr "
  "rr rr rr rr {{ {{ x. .. ,, ,, ,, ,, rr .. {{ rr rr rr rr "
  "rr rr rr rr rr {{ x. ,, ,, ,, ,, ,, x. {{ {{ rr rr rr rr "
  "rr rr rr rr {{ {{ x. ,, ,, ,, ,, ,, x. {{ {{ rr rr rr rr "
  "rr rr rr rr {{ {{ bb ,, ,, ,, ,, .. x. {{ rr rr rr rr rr "
  "rr rr rr rr rr {{ x. ,, ,, ,, .. bb x. rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr x. x. && x. x. x. rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 (put (mk-ladder-down 'p_paladins_hold 3 9) 9 14)
 (put (guard-pt 'halberdier) 8 12)
 (put (guard-pt 'crossbowman) 10 12)
 )

;;
;; assemble the rooms into a dungeon level
;;

(mk-dungeon-level 
 (list nil                 p_shamans_grove     nil                  )
 (list p_kurpolis_entrance p_goblin_crossroads p_cave_goblin_village)
 (list nil                 p_watchpoint        p_trolls_den         )
 )
