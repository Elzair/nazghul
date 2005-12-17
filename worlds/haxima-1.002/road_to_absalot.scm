(kern-load "joel.scm")

(mk-dungeon-room
 'p_road_to_absalot_1 "Passage to Absalot"
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
  (put (mk-ladder-up 'p_deepness 12 8) 9 9)
 (put (spawn-pt 'troll) 7  4)
 (put (spawn-pt 'troll) 14 13)
 (put (spawn-pt 'headless) 14 9)
 (put (spawn-pt 'headless) 14 5)
 (put (spawn-pt 'gazer) 1 3)
 )

(mk-dungeon-room
 'p_fire_bridge "Fire Bridge"
 (list
  "rr rr rr !! !! !! xx xx xx xx xx xx xx xx xx xx xx xx xx "
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
 'p_road_to_absalot_3 "Passage to Absalot"
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
 (put (mk-ladder-up 'p_tower_of_absalot 9 9) 11 9)
 (put (guard-pt 'gazer) 11 9)
 (put (guard-pt 'corrupt-halberdier) 7 9)
 (put (spawn-pt 'headless) 4 8)
 (put (spawn-pt 'zorn) 6 11)
 )


(mk-dungeon-level 
 (list p_road_to_absalot_1 p_fire_bridge p_road_to_absalot_3)
 )

(mk-tower
 'p_gate_to_absalot "Gate To Absalot"
 (list
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ xx xx xx xx xx ^^ ^^ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ xx xx xx xx xx ,, ,, ,, xx xx xx xx xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, ,, ,, xx xx bb xx xx ,, ,, ,, xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, ,, ,, xx bb bb bb xx ,, ,, ,, xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, ,, ,, w+ .. bb .. w+ ,, ,, ,, xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx ,, ,, ,, xx .. .. .. xx ,, ,, ,, xx ^^ ^^ ^^ "
      "^^ ^^ ^^ xx xx w+ xx xx .. .. .. xx xx w+ xx xx ^^ ^^ ^^ "
      "^^ ^^ ^^ {{ .. .. .. .. .. .. .. .. .. .. .. {{ ^^ ^^ ^^ "
      "^^ ^^ ^^ {{ {{ .. .. .. .. .. .. .. .. .. {{ {{ ^^ ^^ ^^ "
      "^^ ^^ ^^ {{ {{ .. .. .. .. && .. .. .. .. {{ {{ ^^ ^^ ^^ "
      "^^ ^^ ^^ {{ {{ .. .. .. .. .. .. .. .. .. {{ {{ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ {{ {{ .. .. .. .. .. .. .. {{ {{ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ {{ {{ .. .. .. .. .. {{ {{ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ ^^ {{ {{ .. .. .. .. .. {{ {{ ^^ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ ^^ {{ {{ .. .. .. .. .. .. .. {{ {{ ^^ ^^ ^^ ^^ "
      "^^ ^^ ^^ {{ {{ .. .. .. .. .. .. .. .. .. {{ {{ ^^ ^^ ^^ "
  )
 (put (mk-ladder-down 'p_road_to_absalot_1 9 9) 9 1)
 (put (mk-joel) 9 10)
 (put (mk-npc 'bull 4) 12 12)
 (put (mk-npc 'bull 4) 10 15)
 )

(mk-tower
 'p_tower_of_absalot "Tower of Absalot"
 (list
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ .. tt tt || || || || || || "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ {{ {{ tt tt || || || || || || "
      "^^ ^^ ^^ {{ bb && bb {{ ^^ ^^ {{ tt tt || || || || || || "
      "^^ ^^ {{ .. .. .. .. .. {{ ^^ {{ tt tt tt || || || || || "
      "^^ ^^ {{ .. .. .. .. .. .. {{ bb .. tt tt tt || || || || "
      "^^ ^^ bb .. .. .. .. .. .. bb {{ .. tt tt tt tt || || || "
      "^^ ^^ ^^ bb .. .. .. .. .. .. bb .. .. tt tt tt tt tt tt "
      "^^ ^^ ^^ ^^ {{ .. .. .. .. .. bb .. .. .. tt tt tt tt tt "
      "^^ ^^ ^^ ^^ bb {{ .. .. .. bb .. .. .. .. .. .. .. .. .. "
      "^^ ^^ ^^ ^^ bb {{ .. .. .. /c /d /d /d /d /d /d /d /d /d "
      "^^ ^^ ^^ ^^ bb bb .. .. .. bb .. .. .. .. .. .. .. .. .. "
      "^^ ^^ ^^ {{ {{ .. .. .. .. .. bb .. .. .. tt tt tt tt tt "
      "^^ ^^ {{ .. .. .. .. .. .. .. bb .. .. tt tt tt tt tt tt "
      "^^ {{ .. .. .. .. .. bb ^^ bb {{ .. tt tt tt tt || || || "
      "^^ {{ .. .. .. .. .. {{ ^^ {{ {{ tt tt tt tt || || || || "
      "^^ {{ .. .. .. .. .. {{ ^^ ^^ {{ tt tt tt || || || || || "
      "^^ {{ {{ .. .. .. {{ ^^ ^^ ^^ {{ {{ tt || || || || || || "
      "^^ ^^ {{ {{ {{ {{ ^^ ^^ ^^ ^^ {{ {{ tt || || || || || || "
      "^^ ^^ ^^ ^^ ^^ ^^ ^^ ^^ .. .. .. {{ tt || || || || || || "
  )
 (put (mk-ladder-down 'p_road_to_absalot_3 9 9) 9 9)
 )
