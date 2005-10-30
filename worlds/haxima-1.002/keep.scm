(kern-load "keep_crypt_mech.scm")

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
                                     

  (put (kern-mk-obj t_mana_potion 1) 11 12)
  (put (kern-mk-obj t_heal_potion 1) 12 11)

  ;; monster generators
  (put (mk-edge-gen 980 4 'is-death-knight? 
                   'mk-death-knight-at-level (list "1d4+4")) 18 9)
  (put (mk-edge-gen 980 3 'is-halberdier? 
                   'mk-at-level
                   (list 'mk-halberdier "1d4+4" nil)) 0 9)
  (put (mk-edge-gen 980 2 'is-crossbowman? 
                   'mk-at-level
                   (list 'mk-crossbowman "1d4+4" nil)) 0 9)
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
  "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx xx xx xx xx xx "
  "xx ,, ,, ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx x! ,, ,, ,, "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ w+ w+ ,, ,, ,, ,, "
  "x! ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ w+ w+ ,, ,, ,, ,, "
  "xx ,, ,, ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx x! ,, ,, ,, "
  "xx xx xx xx xx x! ,, xx xx xx xx xx xx xx xx xx xx xx xx "
  "xx .A .R .M .S xx ,, ,, xx xx xx xx xx .M .E .D .I .K xx "
  "xx ,, ,, ,, ,, x! ,, ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
  "x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! "
  "xx ,, ,, ,, ,, x! ,, ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, xx xx xx ,, xx xx xx xx ,, ,, ,, ,, ,, xx "
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  )
 (put (mk-portcullis) 14 9)
 (put (mk-portcullis) 8 8)
 (put (mk-portcullis) 8 10)
 (put (mk-door) 6 5)
 (put (mk-door) 6 12)
 (put (mk-door) 5 15)
 (put (mk-door) 12 15)
 (put (mk-bed) 13 17)
 (put (mk-bed) 15 17)
 (put (mk-bed) 17 17)
 (put (mk-bed) 17 15)
 (put (mk-ladder-down 'p_forsaken_prison 9 9) 8 17)
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
)

(mk-dungeon-level 
 (list nil              p_treasury   nil)
 (list p_paladins_hold  p_great_hall p_death_knights_hold)
 (list nil              p_keep_crypt nil)
 )
