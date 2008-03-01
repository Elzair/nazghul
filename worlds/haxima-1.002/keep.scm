(kern-load "anne.scm")
(kern-load "jones.scm")
(kern-load "alex.scm")

(mk-dungeon-room
 'p_great_hall "Great Hall"
 (list
  "xx xx xx xx xx xx xx xx xx xx xx xx xx x! xx xx xx xx xx "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
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
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  )

 (put (mk-ladder-down 'p_pools 9 9) 9 6)

  ;; secret wall mech
  (put (mk-hidden-mech) 13 0)
  (put (mk-disg-lvr 'gh_wall 's_wall_torch) 13 0)
  (put (kern-tag 'gh_wall 
                 (mk-tblitter 'p_great_hall
                              8
                              0
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

(mk-place-music p_great_hall 'ml-dungeon-adventure)

(mk-dungeon-room
 'p_paladins_hold "Paladin's Hold"
 (list
  "rn rn xx xx xx rn rn rn rn xx xx xx xx && xx xx xx xx xx "
  "rn xx xx ,, xx xx rn rn xx x! ,, ,, ,, ,, ,, ,, ,, x! xx "
  "xx xx ,, ,, ,, xx x! xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx && ,, ,, ,, ,, ,, ,, ,, ,, ,, [[ @@ @@ @@ ]] ,, ,, xx "
  "xx xx ,, ,, ,, xx ,, xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "rn xx xx xx xx xx ,, xx xx x! ,, ,, ,, ,, ,, ,, ,, x! xx "
  "rn rn xx xx xx xx ,, xx xx xx xx xx xx xx xx xx xx xx xx "
  "rn rn xx x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! ,, ,, ,, ,, "
  "rn rn xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, "
  "rn rn xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, "
  "rn rn xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, "
  "rn rn xx x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! .. ,, ,, ,, "
  "xx xx xx xx xx xx xx xx ,, xx xx xx xx xx xx xx xx xx xx "
  "xx .A .R .M .S xx xx xx ,, xx xx xx xx .M .E .D .I .K xx "
  "xx ,, ,, ,, ,, x! ,, ,, ,, ,, ,, ,, sH ,, ,, ,, ,, ,, xx "
  "x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! "
  "xx ,, ,, ,, ,, sA ,, ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, xx xx xx ,, xx xx xx xx ,, ,, ,, ,, ,, xx "
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  )
 (put (kern-tag 'php1 (mk-portcullis)) 14 9)
 (put (mk-clock) 3 1) 
 (put (mk-door) 5 15)
 (put (mk-door) 12 15)
 (put (mk-bed) 13 17)
 (put (mk-bed) 15 17)
 (put (mk-bed) 17 17)
 (put (mk-bed) 17 15)
 (put (mk-ladder-down 'p_forsaken_prison 9 9) 8 17)
 (put (mk-ladder-up 'p_watchpoint 9 14) 3 9)
 (put (mk-anne) 9 9)
 (put (mk-jones) 9 9)
 (put (mk-alex) 9 9)

 (put (custom-pt 'put-gate-guard 'php1 'deep) 11 9)

 ;; inner guard
 (put (guard-pt 'crossbowman) 13 8)
 (put (guard-pt 'crossbowman) 13 10)

 ;; outer guard
 (put (guard-pt 'halberdier) 15 10)
 (put (guard-pt 'halberdier) 15 8)

 ;; foes...
 (put (spawn-pt 'death-knight) 17 9)
 (put (spawn-pt 'skeletal-spear-thrower) 18 7)
 (put (spawn-pt 'skeletal-spear-thrower) 18 11)

 )

(mk-place-music p_paladins_hold 'ml-castle)

(mk-dungeon-room
 'p_treasury "Treasury"
 (list
  "xx xx xx xx xx rn xx xx xx xx xx xx xx rn xx xx xx xx xx "
  "xx ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx "
  "xx ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx "
  "xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, xx "
  "xx xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx "
  "rn xx xx ,, ,, ,, ,, ,, w+ xx w+ ,, ,, ,, ,, ,, xx xx rn "
  "xx xx xx ,, ,, ,, ,, ,, w+ xx w+ ,, ,, ,, ,, ,, xx xx xx "
  "xx ,, ,, ,, ,, ,, ,, ,, w+ xx w+ ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, xx xx xx x! xx x! xx xx xx ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, xx rn rn xx ,, xx rn rn xx ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, xx xx xx x! xx x! xx xx xx ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, xx rn xx ,, ,, ,, ,, ,, ,, ,, xx "
  "xx xx xx ,, ,, ,, ,, ,, xx rn xx ,, ,, ,, ,, ,, xx xx xx "
  "rn xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx rn "
  "xx xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx xx "
  "xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, xx "
  "xx ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx "
  "xx ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, xx "
  "xx xx xx xx xx rn xx ,, ,, ,, ,, ,, xx rn xx xx xx xx xx "
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

 (put (mk-ladder-up 'p_treasury2 9 9) 9 9)

)

;; slimes in NE corner
(foldr-rect p_treasury 15 1 3 3 
            (lambda (x loc) (kern-obj-put-at (spawn-pt 'green-slime) loc)) 
            #f)

;; skeletal warriors in SE corner
(foldr-rect p_treasury 15 15 3 3 
            (lambda (x loc) (kern-obj-put-at (spawn-pt 'skeletal-warrior) loc)) 
            #f)

;; headless in SW corner
(foldr-rect p_treasury 1 1 3 3 
            (lambda (x loc) (kern-obj-put-at (spawn-pt 'headless) loc)) 
            #f)

;; spiders in NW corner
(foldr-rect p_treasury 1 15 3 3 
            (lambda (x loc) (kern-obj-put-at (spawn-pt 'giant-spider) loc)) 
            #f)

(mk-place-music p_treasury 'ml-dungeon-adventure)


(mk-dungeon-room
 'p_death_knights_hold "Death Knight's Hold"
 (list
		"xx xx xx xx xx xx xx xx xx xx xx r8 r8 r8 r8 r8 rn rn rn "
		"xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. .. bb bb ra rn rn "
		"xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, .. .. .. .. bb ra rn "
		"xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, .. .. .. .. .. bb r2 "
		"xx xx xx xx ,, ,, ,, xx xx xx xx ,, ,, ,, ,, .. .. .. r2 "
		",, ,, ,, xx ,, ,, ,, xx ,, ,, ,, ,, xx xx ,, xx xx xx xx "
		",, ,, ,, xx w+ w+ w+ xx ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
		",, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx "
		",, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, ,, ,, ,, xx "
		",, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, ,, ,, ,, xx "
		",, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, ,, ,, ,, xx "
		",, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, xx "
		",, ,, ,, xx w+ w+ w+ xx ,, ,, ,, ,, xx ,, ,, ,, ,, ,, xx "
		",, ,, ,, xx ,, ,, ,, xx ,, ,, ,, ,, xx xx ,, xx xx xx xx "
		"xx xx xx xx ,, ,, ,, xx xx xx xx ,, ,, ,, ,, .. .. .. r2 "
		"xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, .. .. .. .. .. bb r2 "
		"xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, .. .. .. .. bb rb rn "
		"xx ,, ,, ,, ,, ,, ,, ,, ,, ,, xx .. .. .. bb bb .. bb r2 "
		"xx xx xx xx xx xx xx xx xx xx xx r1 r1 r1 r1 r1 r1 r1 rn "
                  )
 (put (mk-locked-door) 10 2)
 (put (mk-magic-locked-door) 14 5)
 (put (mk-magic-locked-door) 14 13)
 (put (mk-locked-door) 10 16)
 (put (mk-ladder-down 'p_altar_room 17 17) 17 9)

 ;; defenders
 (put (guard-pt 'craven-archer)  5  5)
 (put (guard-pt 'craven-archer)  5 13)
 (put (guard-pt 'craven-archer) 11  9)
 (put (spawn-pt 'death-knight)  14  2)
 (put (spawn-pt 'death-knight)  14 16)
 (put (spawn-pt 'death-knight)   7  9)
 (put (guard-pt 'death-knight)   9  2)
 (put (guard-pt 'death-knight)   9 16)
 (put (guard-pt 'death-knight)  14  6)
 (put (guard-pt 'death-knight)  14 12)
 (put (guard-pt 'warlock)       14  9)

)

(mk-place-music p_death_knights_hold 'ml-dungeon-adventure)

(mk-dungeon-level 
 (list nil              p_treasury   nil)
 (list p_paladins_hold  p_great_hall p_death_knights_hold)
 (list nil              nil          nil)
 )
