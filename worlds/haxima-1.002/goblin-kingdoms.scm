(mk-dungeon-room
 'p_kurpolis_entrance "Entrance to Kurpolis"
 (list
      "rr rr rr rr xx xx x! xx xx && xx xx x! xx xx rr rr rr rr "
      "rr .. .. .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx xx x! xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx ,, ,, ,, ,, ,, ,, [[ @@ @@ @@ ]] ,, ,, x! rr rr rr rr "
      "xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx ,, ,, ,, xx xx xx xx xx x! xx xx xx xx xx rr bb ,, rr "
      "xx ,, ,, ,, xx xx .K .U .R .P .O .L .I .S xx bb bb bb ,, "
      "xx ,, ,, ,, x! ,, ,, ,, ,, ,, ,, ,, ,, ,, x! bb ,, bb ,, "
      "x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ bb ,, ,, ,, "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, bb ,, "
      "xx ,, ,, ,, x! ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ bb bb bb ,, "
      "xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, x! bb ,, bb rr "
      "xx ,, ,, ,, xx xx xx xx xx xx xx xx xx xx xx ,, bb rr rr "
      "xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "xx xx x! xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "rr rr rr rr xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr rr rr rr "
      "rr rr rr rr xx xx xx xx xx xx xx xx xx xx xx rr rr rr rr "
  )
 (put (mk-ladder-up 'p_shard 39 75) 9 9)
 (put (mk-door) 4 15)
 (put (mk-door) 4 3)
 (put (mk-door) 4 1)
 (put (mk-windowed-door) 14 10)
 (put (mk-chest nil (list (list 10 t_food))) 1 1)
 (put (mk-bed) 5 17)
 (put (mk-bed) 7 17)
 (put (mk-bed) 9 17)
 (put (mk-bed) 11 17)
 (put (mk-bed) 13 17)
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
  "rr rr rr rr rr {{ {{ {{ {{ {{ .. {{ {{ {{ {{ rr rr rr rr "
  "rr rr rr rr rr rr rr rr {{ {{ {{ rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 (put (mk-mongen2 250 5 'is-goblin? 'mk-goblin-hunter nil) 18 8)
 (put (mk-mongen2 500 5 'is-goblin? 'mk-goblin-raider nil) 18 10)
 )

(mk-dungeon-room
 'p_grey_goblin_village "Grey Goblin Village"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr {{ {{ {{ rr {{ {{ {{ {{ {{ {{ {{ rr rr rr {{ {{ {{ rr "
  "rr {{ .. {{ rr {{ ,, ,, ,, ,, {{ {{ {{ rr {{ ,, ,, {{ rr "
  "rr {{ ,, ,, {{ ,, ,, ,, && ,, ,, bb {{ {{ {{ ,, ,, {{ rr "
  "rr {{ .. ,, .. .. ,, ,, ,, ,, ,, ,, .. .. .. ,, ,, {{ rr "
  "rr {{ {{ {{ {{ {{ .. .. ,, ,, {{ {{ ,, {{ {{ ,, ,, {{ rr "
  "rr rr rr rr rr {{ {{ {{ .. {{ bb {{ ,, rr {{ .. ,, {{ rr "
  "{{ {{ {{ {{ bb {{ bb {{ .. .. {{ ,, ,, rr {{ .. ,, {{ rr "
  "{{ {{ {{ {{ bb {{ {{ .. ,, ,, ,, ,, {{ rr rr {{ .. {{ rr "
  ",, {{ ,, .. .. .. .. ,, .. ,, ,, ,, {{ {{ rr rr {{ {{ rr "
  ",, ,, .. {{ bb {{ .. ,, ,, ,, ,, .. {{ {{ {{ rr {{ {{ rr "
  "{{ {{ {{ {{ bb {{ {{ ,, ,, ,, ,, ,, .. .. {{ rr rr rr rr "
  "rr rr rr rr rr {{ bb {{ ,, ,, ,, {{ ,, .. .. ~~ ~~ rr rr "
  "rr rr rr rr rr {{ {{ {{ {{ ,, {{ {{ {{ .. ~~ -- -- ~~ rr "
  "rr rr rr rr rr rr rr bb bb .. bb bb rr {{ ~~ -- -- ~~ rr "
  "rr rr rr rr rr rr rr {{ {{ .. {{ {{ rr {{ {{ ~~ ~~ {{ rr "
  "rr rr rr rr rr rr rr {{ {{ .. .. {{ rr rr {{ {{ {{ {{ rr "
  "rr rr rr rr rr rr rr {{ {{ {{ .. {{ rr rr {{ {{ {{ rr rr "
  "rr rr rr rr rr rr rr {{ {{ .. .. {{ rr rr rr rr rr rr rr "
  )
 (put (mk-mongen2 100 10 'is-goblin? 'mk-goblin-hunter nil) 8 4)
 (put (mk-mongen2 100 10 'is-goblin? 'mk-goblin-raider nil) 9 9)
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
 (put (mk-mongen2 900 3 'is-troll? 'mk-troll nil) 16 10)
 )

(mk-dungeon-level 
 (list nil                 nil                 nil                  )
 (list p_kurpolis_entrance p_goblin_crossroads p_grey_goblin_village)
 (list nil                 nil                 p_trolls_den         )
 )
