(mk-dungeon-room
 'p_fire_sea "Fire Sea"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr {{ {{ {{ {{ {{ {{ {{ {{ rr rr rr {{ {{ rr "
  "rr rr {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ rr {{ ~! {{ rr "
  "rr {{ !! !! {{ bb {{ {{ {{ {{ {{ !! {{ {{ {{ ~! !_ ~! rr "
  "rr {{ !! !_ !! !! {{ !! !! {{ !! !_ !! {{ {{ {{ ~! {{ rr "
  "rr {{ !! !! {{ == {{ !! !_ !! !_ !_ !! {{ {{ !! ~! {{ rr "
  "rr {{ {{ {{ {{ !! !! !! !_ rr rr !_ !_ !! {{ == {{ {{ rr "
  "rr {{ {{ {{ !! !_ !_ !_ rr rr rr rr !_ !_ !! !! !! {{ rr "
  "rr {{ {{ {{ !! !_ !_ !_ rr {{ {{ rr rr !_ !_ !! {{ {{ rr "
  "rr {{ {{ {{ {{ !! !! rr rr {{ {{ {{ rr !_ !_ !! {{ {{ rr "
  "rr {{ {{ {{ {{ {{ !! rr rr {{ bb {{ {{ {{ == {{ {{ {{ rr "
  "rr {{ !! {{ !! =| !! rr rr {{ {{ {{ rr !_ !_ !! {{ {{ rr "
  "rr !! !_ !! !! {{ !! !_ rr rr rr {{ rr rr !_ !_ !! {{ rr "
  "rr {{ !! {{ {{ {{ {{ !! !_ !_ rr rr rr rr !_ !_ !_ !! rr "
  "rr rr {{ {{ {{ {{ {{ !! !_ !_ !_ !_ rr rr rr !_ !_ !_ rr "
  "rr rr rr {{ {{ {{ {{ {{ !! !! !_ !_ !_ !_ !_ !_ !_ !_ rr "
  "rr rr rr {{ bb {{ {{ {{ {{ {{ !! !_ !! !! !_ !_ !_ !_ rr "
  "rr rr {{ {{ {{ {{ bb {{ {{ {{ {{ !! {{ {{ !! !_ !_ !_ rr "
  "rr {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ bb !! !! ~! rr "
  )
 (put (kern-mk-obj t_rune_w 1) 11 12)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 9 8)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 10 8)
 (put (kern-mk-obj t_iron_helm_4 1) 10 8)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 9 9)
 (put (kern-mk-obj t_gem (kern-dice-roll "5d4+1")) 10 9)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 11 9)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 9 10)
 (put (kern-mk-obj t_sword_4 1) 9 10)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 11 10)
 (put (kern-mk-obj t_armor_plate_4 1) 11 11)
 (put (kern-mk-obj t_gem (kern-dice-roll "5d4+1")) 9 11)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 10 11)
 (put (kern-mk-obj t_shield_4 1) 10 11)
 (put (kern-mk-obj t_gem (kern-dice-roll "5d4+1")) 11 11)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 11 12)
 )

(mk-dungeon-room
 'p_rivulets_of_fire "Rivulets Of Fire"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr {{ {{ {{ rr rr rr rr {{ {{ {{ {{ {{ rr rr rr "
  "rr rr rr {{ {{ {{ {{ {{ {{ {{ {{ {{ ~! ~! ~! {{ rr rr rr "
  "rr {{ {{ {{ ~! ~! ~! ~! ~! ~! {{ {{ {{ {{ ~! {{ {{ rr rr "
  "rr rr rr {{ {{ {{ {{ {{ {{ ~! ~! ~! ~! ~! ~! ~! ~! ~! ~! "
  "rr rr rr rr {{ {{ {{ bb {{ ~! {{ {{ {{ {{ {{ ~! {{ rr rr "
  "rr rr {{ {{ {{ ~! ~! ~! ~! ~! {{ {{ {{ {{ {{ ~! {{ rr rr "
  "rr rr rr {{ {{ {{ {{ ~p ,, ,, ,, ~p ~! ~! ~! ~! {{ {{ rr "
  "rr rr rr rr rr {{ {{ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  "rr rr rr rr rr rr {{ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  "rr rr rr rr {{ {{ {{ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  "rr rr rr {{ {{ {{ {{ ~p ,, ,, ,, ~p {{ {{ {{ {{ {{ {{ rr "
  "rr rr {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ ~! ~! ~! ~! {{ rr "
  "rr {{ {{ ~! ~! ~! ~! ~! ~! ~! ~! {{ {{ {{ {{ bb ~! {{ rr "
  "rr rr {{ {{ {{ {{ {{ {{ {{ {{ ~! {{ {{ {{ bb {{ ~! rr rr "
  "rr rr rr {{ {{ {{ {{ {{ {{ bb ~! ~! ~! ~! ~! ~! ~! ~! ~! "
  "rr rr rr rr {{ {{ {{ {{ ~! ~! ~! {{ {{ {{ {{ {{ {{ rr rr "
  "rr rr rr {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 )

(mk-dungeon-room
 'p_fire_bridge "Fire Bridge"
 (list
  "rr rr rr !! !! !! xx xx ,, ,, ,, xx xx xx xx xx xx xx xx "
  "rr {{ {{ !! !! xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "rr {{ {{ !! !! w+ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "rr {{ !! !! !! w+ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "rr {{ !! !! !! w+ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "rr {{ !! !! !! xx xx ,, ,, ,, xx xx w+ xx xx ,, ,, ,, xx "
  "rr {{ {{ !! !! !! xx w+ w+ w+ xx !! !! !_ xx ,, ,, ,, xx "
  "rr {{ {{ {{ !! !! !! !! !! !! !! !! !_ !_ xx ,, ,, ,, xx "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, !! !_ !_ ,, ,, ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, !! !_ !_ ,, w+ ,, ,, ,, ,, "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, !! !_ !_ ,, ,, ,, ,, ,, ,, "
  "rr {{ {{ {{ !! !! !! !! !! !! !! !! !_ !_ xx ,, ,, ,, xx "
  "rr {{ {{ !! !! !! xx w+ w+ w+ xx !! !! !_ xx ,, ,, ,, xx "
  "rr {{ !! !! !! xx xx ,, ,, ,, xx xx w+ xx xx ,, ,, ,, xx "
  "rr {{ !! !! !! w+ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "rr {{ !! !! !! w+ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "rr {{ {{ !! !! w+ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "rr rr {{ !! !! xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "rr rr rr !! !! !! xx xx xx xx xx xx xx xx xx xx xx xx xx "
  )
  (put (kern-tag 'fb-p2 (mk-portcullis)) 14 8)
  (put (kern-tag 'fb-p1
                 (mk-connected-portcullis 'fb-p2)) 14 10)
  (put (kern-tag 'fb-b1 (mk-tblitter 'p_fire_bridge
                                     10 8 3 3
                                     'm_deck_section)) 10 8)
  (put (mk-lever 'fb-p1) 8 3)
  (put (mk-lever 'fb-b1) 8 15)
 )


(mk-dungeon-room
 'p_gate_to_absalot "Gate to Absalot"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr {{ {{ {{ {{ !! !! !! !! !! !! !! !! {{ rr rr rr "
  "rr rr {{ {{ {{ !! !! !! !! !! !! !! !! !! !! !! {{ rr rr "
  "rr {{ {{ {{ !! !! !! !! !! {{ {{ {{ {{ {{ !! !! !! {{ rr "
  "rr {{ {{ !! !! !! !! !! {{ {{ {{ {{ {{ {{ {{ !! !! !! rr "
  "xx {{ !! !! !! !! !! {{ {{ {{ xx xx xx {{ {{ {{ !! !! rr "
  "xx xx xx xx !! !! {{ {{ {{ xx xx xx xx xx {{ {{ {{ !! rr "
  "xx ,, ,, xx {{ {{ {{ {{ xx xx xx xx xx xx xx {{ {{ {{ rr "
  ",, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx xx xx {{ {{ rr "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx {{ {{ rr "
  ",, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx xx xx {{ {{ rr "
  "xx ,, ,, xx {{ {{ {{ {{ xx xx xx xx xx xx xx {{ {{ {{ rr "
  "xx xx xx xx !! !! !! {{ {{ xx xx xx xx xx {{ {{ {{ !! rr "
  "xx {{ !! !! !! !! !! {{ {{ {{ xx xx xx {{ {{ {{ {{ !! rr "
  "rr {{ {{ !! !! !! !! !! {{ {{ {{ {{ {{ {{ {{ !! !! !! rr "
  "rr {{ {{ {{ !! !! !! !! !! {{ {{ {{ {{ {{ !! !! !! {{ rr "
  "rr {{ {{ {{ {{ !! !! !! !! !! !! !! !! !! !! !! {{ rr rr "
  "rr rr {{ {{ {{ {{ {{ !! !! !! !! !! !! !! !! {{ rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 (put (mk-magic-locked-door) 8 9)
 )


(mk-dungeon-level 
 (list nil p_fire_sea nil)
 (list p_rivulets_of_fire p_fire_bridge p_gate_to_absalot)
 )
