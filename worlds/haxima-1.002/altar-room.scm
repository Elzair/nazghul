(mk-dungeon-room
 'p_altar_room "Altar Room"
 (list
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
      "xx xx xx xx xx xx xx pp ,, ,, ,, pp xx xx xx xx xx xx xx "
      "xx xx ,, ,, ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, ,, ,, xx xx "
      "xx xx ,, cc cc cc cc cc cc aa cc cc cc cc cc cc ,, xx xx "
      "xx xx ,, cc ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, cc ,, xx xx "
      "xx xx ,, cc ,, xx xx pp ,, cc ,, pp xx xx ,, cc ,, xx xx "
      "xx xx ,, cc ,, xx xx xx ,, cc ,, xx xx xx ,, cc ,, xx xx "
      "xx pp ,, cc ,, pp xx xx vv vv vv xx xx pp ,, cc ,, pp xx "
      "xx ,, cc cc cc ,, ,, vv vv vv vv vv ,, ,, cc cc cc ,, xx "
      "xx ,, cc aa cc cc cc vv vv vv vv vv cc cc cc aa cc ,, xx "
      "xx ,, cc cc cc ,, ,, vv vv vv vv vv ,, ,, cc cc cc ,, xx "
      "xx pp ,, cc ,, pp xx xx vv vv vv xx xx pp ,, cc ,, pp xx "
      "xx xx ,, cc ,, xx xx xx ,, cc ,, xx xx xx ,, cc ,, xx xx "
      "xx xx ,, cc ,, xx xx pp ,, cc ,, pp xx xx ,, cc ,, xx xx "
      "xx xx ,, cc ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, cc ,, xx xx "
      "xx xx ,, cc cc cc cc cc cc aa cc cc cc cc cc cc ,, xx xx "
      "xx xx ,, ,, ,, ,, ,, ,, cc cc cc ,, ,, ,, ,, ,, ,, ,, xx "
      "xx xx xx xx xx xx xx pp ,, ,, ,, pp xx xx xx xx ,, ,, xx "
      "xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx "
  )
 (put (mk-ladder-up 'p_death_knights_hold 17 9) 17 17)

 ;; boss L20 warlock (first time only)
 (put (mk-npc 'warlock 20) 9 6)

 ;; subservient warlocks (respawn)
 (put (spawn-pt 'warlock) 9 12)
 (put (spawn-pt 'warlock) 6 9)
 (put (spawn-pt 'warlock) 12 9)
 )

(mk-place-music p_altar_room 'ml-dungeon-adventure)