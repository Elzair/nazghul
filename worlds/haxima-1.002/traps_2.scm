;; ----------------------------------------------------------------------------
;; Level 2 of the Thief's Ladder
;; ----------------------------------------------------------------------------
(mk-dungeon-room
 'p_traps_2 "The Choice"
 (list
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, xx xx xx ,, ,, ,, ,, ,, ,, ,, xx "
  "xx xx ,, ,, ,, ,, ,, xx xx xx xx xx ,, ,, ,, ,, ,, xx xx "
  "xx xx xx ,, ,, ,, xx xx ,, ,, ,, xx xx ,, ,, ,, xx xx xx "
  "xx xx xx xx ,, xx xx ,, ,, ,, ,, ,, xx xx ,, xx xx xx xx "
  "xx xx xx ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, xx xx xx "
  "xx xx xx ,, ,, ,, xx ,, ,, ,, ,, ,, xx ,, ,, ,, xx xx xx "
  "xx xx xx xx ,, xx xx xx xx ~x xx xx xx xx ,, xx xx xx xx "
  "xx xx xx xx ,, xx xx .C .H ~O .O .S .E xx ,, xx xx xx xx "
  "xx xx xx xx ,, xx xx !! !! ~! !! !! !! xx ,, xx xx xx xx "
  "xx xx xx xx ,, xx xx ,, ,, ,, ,, ,, ,, xx ,, xx xx xx xx "
  "xx xx xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx xx xx "
  "xx xx xx xx xx xx xx ,, ,, ,, ,, ,, ,, xx xx xx xx xx xx "
  "xx xx xx xx xx xx xx !! !! !! !! !! !! xx xx xx xx xx xx "
  "xx xx xx xx xx xx xx .W .I .S .E .L .Y xx xx xx xx xx xx "
  )
 (put (mk-step-clue "Doesn't the other way look better?") 13 15)
 (put (mk-step-clue "Doesn't the other way look better?") 6 15)

 ;; secret path through wall
 (put (mk-secret-path) 9 14)
 (put (mk-secret-path) 9 13)
 (put (mk-secret-path) 9 12)
 (put (mk-secret-path) 9 11)
 (put (mk-secret-path) 9 10)
 (put (mk-secret-path) 9 9)
 
 ;; ladders "down" that link to each other
 (put (mk-ladder-down 'p_traps_2 14 3) 4 3)
 (put (mk-ladder-down 'p_traps_2 4 3) 14 3)
 
 ;; monster generators
 (put (spawn-pt 'bandit) 4 3)
 (put (spawn-pt 'bomber) 3 4)
 (put (spawn-pt 'blackguard) 5 4)

 (put (spawn-pt 'skeletal-warrior) 14 4)
 (put (spawn-pt 'skeletal-spear-thrower) 13 3)
 (put (spawn-pt 'skeletal-archer) 15 3)
 
 ;; doors
 (put (mk-door) 14 8)
 (put (mk-door) 4 8)
 
 ;; true ladder down
 (put (mk-ladder-down 'p_traps_3 9 9) 9 9)
 (put (mk-ladder-up 'p_traps_1 14 2) 9 15)
 )
 
(mk-place-music p_traps_2 'ml-dungeon-adventure)

(kern-place-add-on-entry-hook p_traps_2 'quest-thiefrune-den2)
