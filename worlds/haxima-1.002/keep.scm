(kern-load "keep_crypt_mech.scm")
(kern-load "anne.scm")
(kern-load "jones.scm")
(kern-load "alex.scm")

(mk-dungeon-room
 'p_keep_crypt "Crypt"
 (list
  "xx xx xx xx xx xx x! ,, ,, ,, ,, ,, x! xx xx xx xx xx xx "
  "xx xx xx ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, xx ,, xx "
  "xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, xx "
  "xx xx xx ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, xx ,, xx "
  "xx xx xx xx xx xx x! ,, ,, ,, ,, ,, x! xx xx xx xx ?? xx "
  "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
  "xx xx xx ,, xx xx x! .C .R .Y .P .T x! xx xx ,, xx xx xx "
  "xx .. .. ,, .. .. xx xx xx xx xx xx xx .. .. ,, .. .. xx "
  "xx .. .. ,, .. .. xx xx xx ,, xx xx xx .. .. ,, .. .. xx "
  "xx .. .. ,, .. .. xx xx ,, ,, ,, xx xx .. .. ,, .. .. xx "
  "xx .. .. ,, .. .. xx xx ,, ,, ,, xx xx .. .. ,, .. .. xx "
  "xx .. .. ,, .. .. x! xx xx ,, xx xx x! .. .. ,, .. .. xx "
  "xx .. .. ,, .. .. xx ,, ,, ,, ,, ,, xx .. .. ,, .. .. xx "
  "xx .. .. ,, .. .. xx ,, ,, ,, ,, ,, xx .. .. ,, .. .. xx "
  "xx .. .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, .. .. xx "
  "xx .. .. .. .. .. xx ,, ,, ,, ,, ,, xx .. .. .. .. .. xx "
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  )
 ;; special mechanisms for turning corpses into skeletal warriors
 (put (kern-tag 'kc_kcma (mk-kcm (mk-rect 1 9 5 9))) 0 0)
 (put (kern-tag 'kc_kcmb (mk-kcm (mk-rect 13 9 5 9))) 0 0)

 (put (mk-ladder-down 'p_lich_tomb 9 16) 9 10)
 
 ;; mundane mechs
 (put (mk-locked-door) 6 2)
 (put (mk-magic-locked-door) 12 2)
 (put (mk-door) 6 6)
 (put (mk-door) 12 6)
 (put (mk-windowed-door) 15 8)
 (put (mk-windowed-door) 3 8)
 (put (kern-tag 'kc_pa (mk-connected-portcullis 'kc_kcma)) 6 16)
 (put (kern-tag 'kc_pb (mk-connected-portcullis 'kc_kcmb)) 12 16)
 (put (kern-tag 'kc_pc (mk-portcullis)) 9 13)
 (put (mk-lever 'kc_pb) 4 2)
 (put (mk-lever 'kc_pa) 14 2)
 (put (mk-lever 'kc_pc) 17 1)
 
 ;; corpses
 (put (mk-corpse) 1  9)
 (put (mk-corpse) 2  9)
 (put (mk-corpse) 4  9)
 (put (mk-corpse) 5  9)
 (put (mk-corpse) 13 9)
 (put (mk-corpse) 14 9)
 (put (mk-corpse) 16 9)
 (put (mk-corpse) 17 9)
 (put (mk-corpse) 1  10)
 (put (mk-corpse) 2  10)
 (put (mk-corpse) 4  10)
 (put (mk-corpse) 5  10)
 (put (mk-corpse) 13 10)
 (put (mk-corpse) 14 10)
 (put (mk-corpse) 16 10)
 (put (mk-corpse) 17 10)
 (put (mk-corpse) 1  11)
 (put (mk-corpse) 2  11)
 (put (mk-corpse) 4  11)
 (put (mk-corpse) 5  11)
 (put (mk-corpse) 13 11)
 (put (mk-corpse) 14 11)
 (put (mk-corpse) 16 11)
 (put (mk-corpse) 17 11)
 (put (mk-corpse) 1  12)
 (put (mk-corpse) 2  12)
 (put (mk-corpse) 4  12)
 (put (mk-corpse) 5  12)
 (put (mk-corpse) 13 12)
 (put (mk-corpse) 14 12)
 (put (mk-corpse) 16 12)
 (put (mk-corpse) 17 12)
 (put (mk-corpse) 1  13)
 (put (mk-corpse) 2  13)
 (put (mk-corpse) 4  13)
 (put (mk-corpse) 5  13)
 (put (mk-corpse) 13 13)
 (put (mk-corpse) 14 13)
 (put (mk-corpse) 16 13)
 (put (mk-corpse) 17 13)
 (put (mk-corpse) 1  14)
 (put (mk-corpse) 2  14)
 (put (mk-corpse) 4  14)
 (put (mk-corpse) 5  14)
 (put (mk-corpse) 13 14)
 (put (mk-corpse) 14 14)
 (put (mk-corpse) 16 14)
 (put (mk-corpse) 17 14)
 (put (mk-corpse) 1  15)
 (put (mk-corpse) 2  15)
 (put (mk-corpse) 4  15)
 (put (mk-corpse) 5  15)
 (put (mk-corpse) 13 15)
 (put (mk-corpse) 14 15)
 (put (mk-corpse) 16 15)
 (put (mk-corpse) 17 15)
 (put (mk-corpse) 1  16)
 (put (mk-corpse) 2  16)
 (put (mk-corpse) 16 16)
 (put (mk-corpse) 17 16)
 (put (mk-corpse) 1  17)
 (put (mk-corpse) 2  17)
 (put (mk-corpse) 3  17)
 (put (mk-corpse) 4  17)
 (put (mk-corpse) 5  17)
 (put (mk-corpse) 13 17)
 (put (mk-corpse) 14 17)
 (put (mk-corpse) 15 17)
 (put (mk-corpse) 16 17)
 (put (mk-corpse) 17 17)
 )

(mk-dungeon-room
 'p_great_hall "Great Hall"
 (list
  "xx xx xx xx xx xx xx ,, ,, ,, ,, ,, xx xx xx xx xx xx xx "
  "xx ,, ,, ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, xx xx xx xx xx ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, xx xx ,, cc cc cc ,, xx xx ,, ,, ,, ,, xx "
  "x! ,, ,, ,, ,, xx x! ,, cc cc cc ,, x! xx ,, ,, ,, ,, x! "
  ",, ,, ,, ,, ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, ,, ,, ,, ,, "
  "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc "
  "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc "
  "cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc cc "
  ",, ,, ,, ,, ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, ,, ,, ,, ,, "
  "x! ,, ,, ,, ,, xx x! ,, cc cc cc ,, x! xx ,, ,, ,, ,, x! "
  "xx ,, ,, ,, ,, xx xx ,, cc cc cc ,, xx xx ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, ,, ,, ,, xx "
  "xx xx xx xx xx xx x! ,, cc cc cc ,, x! xx xx xx xx xx xx "
  )

  ;; secret wall mech
  (put (make-invisible (mk-lever 'gh_wall)) 12 2)
  (put (kern-tag 'gh_wall 
                 (mk-tblitter 'p_great_hall
                              8
                              2
                              3
                              1
                              'm_hall_section))
       0
       0)
                                     
  ;; militia
  (put (spawn-pt 'halberdier) 8 8)
  (put (spawn-pt 'halberdier) 8 10)
  (put (spawn-pt 'crossbowman) 6 8)
  (put (spawn-pt 'medik) 7 9)
  (put (spawn-pt 'crossbowman) 6 10)

  ;; death knights
  (put (spawn-pt 'death-knight) 10 8)
  (put (spawn-pt 'death-knight) 10 9)
  (put (spawn-pt 'death-knight) 10 10)
  (put (spawn-pt 'warlock)      12 9)

  ;; skeletons
  (put (spawn-pt 'skeletal-warrior) 9 13)
  (put (spawn-pt 'skeletal-spear-thrower) 10 14)
  )

(mk-dungeon-room
 'p_paladins_hold "Paladin's Hold"
 (list
  "xx xx xx xx xx xx xx xx xx xx xx x! xx && xx x! xx xx x! "
  "xx xx ,, ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx && ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx && ,, ,, ,, ,, ,, ,, ,, ,, ,, [[ @@ @@ @@ ]] ,, ,, x! "
  "xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx xx xx xx xx x! ,, xx x! ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, xx ,, ,, ,, ,, ,, xx xx xx xx xx xx x! xx xx xx xx "
  "xx ,, xx ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, "
  "x! ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  "xx ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, "
  "xx ,, xx ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, w+ .. ,, ,, ,, "
  "xx xx xx xx xx x! ,, xx xx xx xx xx xx xx x! xx xx xx xx "
  "xx .A .R .M .S xx ,, ,, xx xx xx xx xx .M .E .D .I .K xx "
  "xx ,, ,, ,, ,, x! ,, ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
  "x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! "
  "xx ,, ,, ,, ,, x! ,, ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, xx xx xx ,, xx xx xx xx ,, ,, ,, ,, ,, xx "
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  )
 (put (kern-tag 'php1 (mk-portcullis)) 14 9)
 (put (kern-tag 'php2 (mk-portcullis)) 8 8)
 (put (kern-tag 'php3 (mk-connected-portcullis 'php2)) 8 10)
 (put (mk-lever 'php3) 1 6)
 (put (mk-lever 'php1) 1 11)
 (put (mk-door) 6 5)
 (put (mk-door) 6 12)
 (put (mk-door) 5 15)
 (put (mk-door) 12 15)
 (put (mk-bed) 13 17)
 (put (mk-bed) 15 17)
 (put (mk-bed) 17 17)
 (put (mk-bed) 17 15)
 (put (mk-ladder-down 'p_forsaken_prison 9 9) 8 17)
 (put (mk-ladder-up 'p_kurpolis_entrance 2 9) 3 6)
 (put (mk-anne) 9 9)
 (put (mk-jones) 9 9)
 (put (mk-alex) 9 9)

 (put (custom-pt 'put-gate-guard 'php3 'deep) 6 9)
 (put (custom-pt 'put-gate-guard 'php1 'deep) 11 9)

 ;; inner guard
 (put (guard-pt 'crossbowman) 9 7)
 (put (guard-pt 'crossbowman) 9 9)
 (put (guard-pt 'crossbowman) 9 11)

 ;; outer guard
 (put (guard-pt 'halberdier) 15 10)
 (put (guard-pt 'halberdier) 15 8)

 ;; foes...
 (put (spawn-pt 'death-knight) 17 9)
 (put (spawn-pt 'skeletal-spear-thrower) 18 7)
 (put (spawn-pt 'skeletal-spear-thrower) 18 11)

 )

(mk-dungeon-room
 'p_treasury "Treasury"
 (list
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  "xx ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx "
  "xx ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx "
  "xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, xx "
  "xx xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx "
  "xx xx xx ,, ,, ,, ,, ,, w+ xx w+ ,, ,, ,, ,, ,, xx xx xx "
  "xx xx xx ,, ,, ,, ,, ,, w+ xx w+ ,, ,, ,, ,, ,, xx xx xx "
  "xx ,, ,, ,, ,, ,, ,, ,, w+ xx w+ ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, xx xx xx x! xx x! xx xx xx ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, xx xx xx xx ,, xx xx xx xx ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, xx xx xx x! xx x! xx xx xx ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
  "xx xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx xx "
  "xx xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx xx "
  "xx xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx "
  "xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, xx "
  "xx ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx "
  "xx ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx "
  "xx xx xx xx xx xx xx ,, ,, ,, ,, ,, xx xx xx xx xx xx xx "
  )
 ;; four corner blitters and levers
 (put (kern-tag 'tr-ba
                (mk-tblitter 'p_treasury
                             2 2
                             3 3
                             'm_hall_section)) 2 2)
 (put (kern-tag 'tr-bb
                (mk-tblitter 'p_treasury
                             14 2
                             3 3
                             'm_hall_section)) 14 2)
 (put (kern-tag 'tr-bc
                (mk-tblitter 'p_treasury
                             14 14
                             3 3
                             'm_hall_section)) 14 14)
 (put (kern-tag 'tr-bd
                (mk-tblitter 'p_treasury
                             2 14
                             3 3
                             'm_hall_section)) 2 14)
 (put (mk-lever 'tr-ba) 11 11)
 (put (mk-lever 'tr-bb) 7 11)
 (put (mk-lever 'tr-bc) 7 7)
 (put (mk-lever 'tr-bd) 11 7)
      
 ;; four center blitters and levers
 (put (kern-tag 'tr-be1
                (mk-tblitter 'p_treasury
                             9 5
                             1 1
                             'm_hall_section)) 9 5)
 (put (kern-tag 'tr-be2
                (mk-tblitter 'p_treasury
                             9 6
                             1 1
                             'm_hall_section)) 9 6)
 (put (kern-tag 'tr-be3
                (mk-tblitter 'p_treasury
                             9 7
                             1 1
                             'm_hall_section)) 9 7)
 (put (kern-tag 'tr-be4
                (mk-tblitter 'p_treasury
                             9 8
                             1 1
                             'm_hall_section)) 9 8)
 (put (mk-lever 'tr-be1) 1 1)
 (put (mk-lever 'tr-be2) 17 1)
 (put (mk-lever 'tr-be3) 17 17)
 (put (mk-lever 'tr-be4) 1 17)
 )

(mk-dungeon-room
 'p_death_knights_hold "Death Knight's Hold"
 (list
  "xx xx xx xx xx xx xx xx xx xx xx rr rr rr rr rr rr rr rr "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. .. bb bb rr rr rr "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, .. .. .. .. bb rr rr "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, .. .. .. .. .. bb rr "
  "xx xx xx xx ,, ,, ,, xx xx xx xx ,, ,, ,, ,, .. .. .. rr "
  ",, ,, ,, xx ,, ,, ,, xx ,, ,, ,, ,, xx xx ,, xx xx xx xx "
  ",, ,, ,, xx w+ w+ w+ xx ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, ,, ,, ,, xx "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, ,, ,, ,, xx "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, ,, ,, ,, xx "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx "
  ",, ,, ,, xx w+ w+ w+ xx ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
  ",, ,, ,, xx ,, ,, ,, xx ,, ,, ,, ,, xx xx ,, xx xx xx xx "
  "xx xx xx xx ,, ,, ,, xx xx xx xx ,, ,, ,, ,, .. .. .. rr "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, .. .. .. .. .. bb rr "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, .. .. .. .. bb rr rr "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. .. bb bb .. bb rr "
  "xx xx xx xx xx xx xx xx xx xx xx rr rr rr rr rr rr rr rr "
                  )
 (put (mk-locked-door) 10 2)
 (put (mk-magic-locked-door) 14 5)
 (put (mk-magic-locked-door) 14 13)
 (put (mk-locked-door) 10 16)
 (put (mk-ladder-down 'p_altar_room 17 17) 17 9)
)

(mk-dungeon-level 
 (list nil              p_treasury   nil)
 (list p_paladins_hold  p_great_hall p_death_knights_hold)
 (list nil              p_keep_crypt nil)
 )
