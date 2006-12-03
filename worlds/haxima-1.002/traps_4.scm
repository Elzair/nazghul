;; ----------------------------------------------------------------------------
;; Level 4 of the Thief's Ladder
;; ----------------------------------------------------------------------------
(mk-dungeon-room
 'p_traps_4 "Labyrinth of Burning Glass"
 (list
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ~, ,, ,, ,, ,, ,, xx "
  "xx ,, pp ~, pp ~, pp ~, pp ~, pp ~, pp ,, pp ~, pp ,, xx "
  "xx ,, ~, ,, ,, ,, ,, ,, ,, ,, ,, ,, ~, ,, ,, ,, ~, ,, xx "
  "xx ,, pp ,, pp ~, pp ~, pp ~, pp ,, pp ~, pp ,, pp ,, xx "
  "xx ,, ~, ,, ~, ,, ,, ,, ~, ,, ,, ,, ~, ,, ,, ,, ~, ,, xx "
  "xx ,, pp ,, pp ~, pp ,, pp ~, ~p ~, pp ,, pp ~, pp ,, xx "
  "xx ,, ~, ,, ,, ,, ~, ,, ~, ,, ,, ,, ~, ,, ~, ,, ~, ,, xx "
  "xx ,, pp ~, pp ,, pp ,, pp ~, pp ~, pp ~, pp ,, pp ,, xx "
  "xx ,, ~, ,, ,, ,, ~, ,, ,, ,, ,, ,, ,, ,, ~, ,, ~, ,, xx "
  "xx ,, pp ,, pp ~, pp ~, pp ~, pp ~, pp ,, pp ,, pp ,, xx "
  "xx ,, ~, ,, ~, ,, ~, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, pp ,, pp ,, pp ,, pp ~, pp ~, pp ,, pp ~, pp ,, xx "
  "xx ,, ~, ,, ~, ,, ~, ,, ~, ,, ,, ,, ~, ,, ,, ,, ~, ,, xx "
  "xx ,, pp ,, pp ,, pp ,, pp ,, pp ,, pp ~, pp ~, pp ,, xx "
  "xx ,, ~, ,, ~, ,, ,, ,, ~, ,, ~, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, pp ,, pp ~, pp ~, pp ~, pp ~, pp ~, pp ~, pp ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  )
 (put (mk-ladder-up 'p_traps_3 9 3) 9 9)
 (put (mk-ladder-down 'p_thiefs_den 9 9) 9 7)
 )
