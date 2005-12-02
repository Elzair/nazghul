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
  "rr {{ {{ {{ !! !! !! !! !! !! {{ {{ {{ bb !! {{ {{ rr rr "
  "rr rr rr {{ {{ {{ {{ {{ bb !! !! !! !! !! !! !! !! !! !! "
  "rr rr rr rr {{ {{ {{ bb bb !! {{ {{ {{ {{ {{ !! {{ rr rr "
  "rr rr {{ {{ {{ !! !! !! !! !! {{ {{ {{ {{ bb !! {{ rr rr "
  "rr rr rr {{ {{ {{ {{ pp ,, ,, ,, pp !! !! !! !! {{ {{ rr "
  "rr rr rr rr rr {{ {{ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  "rr rr rr rr rr rr {{ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  "rr rr rr rr {{ {{ {{ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  "rr rr rr {{ {{ {{ {{ pp ,, ,, ,, pp {{ {{ {{ {{ {{ {{ rr "
  "rr rr {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ !! !! !! !! {{ rr "
  "rr {{ {{ !! !! !! !! !! !! !! !! {{ {{ {{ {{ bb !! {{ rr "
  "rr rr {{ {{ {{ {{ {{ {{ bb bb !! {{ {{ {{ bb {{ !! rr rr "
  "rr rr rr {{ {{ {{ {{ {{ {{ bb !! !! !! !! !! !! !! !! !! "
  "rr rr rr rr {{ {{ {{ {{ !! !! !! {{ {{ {{ {{ {{ {{ rr rr "
  "rr rr rr bb {{ {{ {{ {{ {{ {{ {{ {{ {{ rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 (put (spawn-pt 'troll) 7  4)
 (put (spawn-pt 'troll) 14 13)
 (put (spawn-pt 'headless) 14 9)
 (put (spawn-pt 'headless) 14 5)
 (put (spawn-pt 'gazer) 1 3)
 )

(mk-dungeon-room
 'p_fire_bridge "Fire Bridge"
 (list
  "rr rr rr !! !! !! xx xx xx ,, xx xx xx xx xx xx xx xx xx "
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
  (put (mk-magic-locked-door) 9 0)
  (put (guard-pt 'corrupt-crossbowman) 8 5)
  (put (guard-pt 'corrupt-crossbowman) 7 5)
  (put (guard-pt 'corrupt-crossbowman) 9 5)

  (put (guard-pt 'corrupt-crossbowman) 7 13)
  (put (guard-pt 'corrupt-crossbowman) 8 13)
  (put (guard-pt 'corrupt-crossbowman) 9 13)

  (put (guard-pt 'corrupt-halberdier) 15 8)
  (put (guard-pt 'corrupt-halberdier) 15 10)
  (put (spawn-pt 'gazer) 16 2)
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
 (put (mk-ladder-up 'p_absalot 40 35) 11 9)
 )

; (put (guard-pt 'gazer) 11 9)
; (put (guard-pt 'corrupt-halberdier) 7 9)
; (put (spawn-pt 'headless) 4 8)
; (put (spawn-pt 'zorn) 6 11)

(mk-dungeon-level 
 (list nil p_fire_sea nil)
 (list p_rivulets_of_fire p_fire_bridge p_gate_to_absalot)
 )
