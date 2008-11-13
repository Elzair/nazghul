;; ----------------------------------------------------------------------------
;; Level 4 of the Thief's Ladder
;; ----------------------------------------------------------------------------
(mk-dungeon-room
 'p_traps_4 "Labyrinth of Burning Glass"
 (list
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ~, ,, ,, ,, ,, ,, xx "
  "xx ,, ,B ~, ,B ~, ,B ~, ,B ~, ,B ~, ,C ,, ,A ~, ,B ,, xx "
  "xx ,, ~, ,, ,, ,, ,, ,, ,, ,, ,, ,, ~, ,, ,, ,, ~, ,, xx "
  "xx ,, ,B ,, ,B ~, ,B ~, ,C ~, ,A ,, ,C ~, ,A ,, ,B ,, xx "
  "xx ,, ~, ,, ~, ,, ,, ,, ~, ,, ,, ,, ~, ,, ,, ,, ~, ,, xx "
  "xx ,, ,B ,, ,B ~, ,B ,, ,C ~, ?B ~, ,C ,, ,B ~, ,C ,, xx "
  "xx ,, ~, ,, ,, ,, ~, ,, ~, ,, ,, ,, ~, ,, ~, ,, ~, ,, xx "
  "xx ,, ,C ~, ,A ,, ,B ,, ,B ~, ,B ~, ,C ~, ,C ,, ,B ,, xx "
  "xx ,, ~, ,, ,, ,, ~, ,, ,, ,, ,, ,, ,, ,, ~, ,, ~, ,, xx "
  "xx ,, ,B ,, ,B ~, ,D ~, ,B ~, ,B ~, ,A ,, ,A ,, ,A ,, xx "
  "xx ,, ~, ,, ~, ,, ~, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,B ,, ,B ,, ,B ,, ,B ~, ,B ~, ,B ,, ,A ~, ,B ,, xx "
  "xx ,, ~, ,, ~, ,, ~, ,, ~, ,, ,, ,, ~, ,, ,, ,, ~, ,, xx "
  "xx ,, ,B ,, ,B ,, ,A ,, ,B ,, ,A ,, ,B ~, ,B ~, ,B ,, xx "
  "xx ,, ~, ,, ~, ,, ,, ,, ~, ,, ~, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx ,, ,A ,, ,B ~, ,B ~, ,C ~, ,C ~, ,B ~, ,B ~, ,A ,, xx "
  "xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx "
  "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  )
 (put (mk-ladder-up 'p_traps_3 9 3) 9 9)
 (put (mk-ladder-down 'p_thiefs_den 9 9) 9 7)
 )

(mk-place-music p_traps_4 'ml-dungeon-adventure)

(kern-place-add-on-entry-hook p_traps_4 'quest-thiefrune-den4)
