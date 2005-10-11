;;----------------------------------------------------------------------------
;; Troll Cave
;;
;; Big underground complex; created by some civilized race, now a ruin
;; inhabited by trolls and other monsters.
;;----------------------------------------------------------------------------


(mk-dungeon-room
 'p_lost_halls_1 "Lost Halls Entrance"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr xx xx rr .. rr rr .. .. rr rr rr rr rr rr rr rr rr rr "
  "rr xx cc cc .. .. .. .. .. .. [[ @@ ]] rr rr rr rr rr rr "
  "rr rr cc cc .. .. .. .. .. .. .. .. .. .. .. rr rr rr rr "
  "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr rr "
  "rr .. .. .. .. .. .. .. .. .. .. && .. .. .. rr .. .. rr "
  "rr .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. rr "
  "rr .. .. .. .. .. .. .. .. .. bb .. bb .. .. .. .. .. .. "
  "rr rr .. .. .. .. && .. .. .. .. .. .. .. .. .. .. .. ,, "
  "rr rr rr .. .. .. .. .. .. .. .. %% %% rr rr xx .. ,, ,, "
  "rr rr rr .. .. .. .. .. .. %% %% %% rr rr xx rr rr ,, ,, "
  "rr rr rr .. .. .. .. .. rr rr ~~ ~~ rr xx rr ,, ,, ,, ,, "
  "rr rr rr rr .. .. rr rr rr rr -- -- xx rr ,, ,, ,, ,, rr "
  "rr rr rr rr .. .. rr rr rr rr rr rr xx ,, ,, ,, ,, rr rr "
  "rr rr rr .. .. .. rr rr rr rr rr xx rr ,, ,, ,, ,, rr rr "
  "rr rr rr .. .. .. .. .. .. .. rr xx rr ,, ,, ,, ,, rr rr "
  "rr rr rr .. .. .. .. .. .. .. rr xx rr rr rr && rr rr rr "
  "rr rr rr rr .. .. rr rr .. .. .. rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr .. .. .. ,, rr ,, ,, ,, rr rr rr "
  )
 (put (mk-corpse2 (mk-treasure-list 3)) 9 10)
 )

(mk-dungeon-room
 'p_lost_halls_2 "Lost Halls East"
 (list
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr .. .. .. rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr .. .. .. .. .. rr rr rr rr "
  "rr .. rr rr rr rr rr rr rr .. .. .. .. .. .. .. .. .. rr "
  "rr .. .. .. rr rr rr .. .. .. .. .. .. rr .. .. .. .. rr "
  "rr .. .. .. .. .. .. .. .. .. .. .. .. rr .. .. .. .. rr "
  "rr .. .. .. .. .. .. .. .. .. .. .. .. rr .. .. .. .. rr "
  ".. .. xx xx .. .. .. .. .. .. .. .. rr rr .. rr rr .. rr "
  ".. .. .X .. .O .. xx xx xx .. .. rr rr rr .. rr rr .. rr "
  ",, ,, ,, ,, ,, ,, xx ,, ,, xx xx .. rr rr .. rr rr .. .. "
  ".. ,, ,, ,, ,, ,, xx ,, ,, ,, ,, xx .. .. .. .. .. ,, rr "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, cc ,, ,, ,, ,, ,, ,, rr "
  "rr rr ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, xx .. rr .. rr rr "
  "rr rr rr ,, ,, ,, rr rr rr ,, ,, ,, rr xx rr .. .. .. rr "
  "rr rr rr rr ,, ,, rr rr rr rr ,, ,, rr rr xx .. .. .. rr "
  "rr rr tt tt ,, ,, rr rr rr rr ,, ,, rr rr xx rr .. .. rr "
  "rr tt .. .. .. ,, rr rr rr rr ,, ,, ,, rr xx rr rr .. rr "
  "rr xx xx xx .. .. .. ,, ,, ,, ,, ,, ,, ,, ,, xx rr .. rr "
  "rr xx rr rr rr rr bb ,, ,, ,, ,, ,, rr rr rr xx xx rr rr "
  )
 )

(mk-dungeon-room
 'p_lost_halls_3 "Lost Halls Keep"
 (list
  "rr rr rr rr rr rr rr rr rr rr xx xx xx xx xx xx xx xx rr "
  "rr rr rr rr rr .. .. rr rr rr xx ,L ,A ,R ,D ,E ,R xx rr "
  "rr rr rr .. .. .. .. .. rr rr xx ,, ,, ,, ,, ,, ,, xx rr "
  "rr rr .. .. .. .. .. .. rr rr xx ,, ,, ,, ,, ,, ,, xx rr "
  "rr rr rr .. .. .. .. bb rr rr xx ,, ,, ,, ,, ,, ,, xx rr "
  "rr rr rr rr rr .. rr rr rr rr xx xx xx ,, ,, xx xx xx rr "
  "rr rr rr ,, ,, ,, ,, ,, rr xx xx xx xx ,, ,, xx xx xx rr "
  "rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr "
  "rr rr ,, ,, pp ,, pp ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr "
  ",, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr "
  "rr rr .. ,, pp ,, pp ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr "
  "rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr "
  "rr rr rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rr "
  "rr rr rr rr rr rr rr rr xx xx xx ,, ,, xx xx xx xx xx rr "
  "rr rr rr rr rr rr rr rr xx xx xx ,, ,, xx xx xx xx rr rr "
  "rr rr xx xx xx xx rr rr xx ,, ,, ,, ,, ,, ,, xx xx rr rr "
  "xx xx xx xx xx xx xx xx xx ,, ,, ,, ,, ,, ,, xx xx rr rr "
  "xx xx xx xx xx xx xx xx xx ,, ,, ,, ,, ,, ,, xx xx rr rr "
  "xx xx xx xx xx xx xx xx ,, ,, ,, xx xx xx xx xx xx rr rr "
  )
 )

(mk-dungeon-room
 'p_lost_halls_4 "Lost Halls South"
 (list
  "rr rr rr rr rr rr rr rr .. .. .. ,, .. rr ,, .. ,, rr rr "
  "rr rr rr rr .. .. .. rr .. .. .. ,, .. ,, ,, ,, ,, ,, rr "
  "rr rr .. .. .. .. .. .. .. .. xx ,, ,, .. ,, ,, ,, ,, rr "
  "rr rr .. .. .. .. .. .. .. .. .. xx rr ,, ,, ,, ,, ,, rr "
  "rr .. .. .. bb .. .. .. .. .. .. xx rr rr rr ,, ,, ,, rr "
  "rr .. .. .. .. .. .. .. bb .. .. rr xx rr rr ,, bb ,, rr "
  "rr rr .. .. .. .. .. .. .. .. .. rr xx rr rr ,, ,, ,, rr "
  "rr rr .. .. .. .. .. .. .. .. rr rr rr xx rr rr ,, ,, ,, "
  "rr rr rr .. .. .. .. .. rr rr rr rr rr rr xx ,, ,, ,, ,, "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr xx xx ,, ,, "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr ,, .. "
  "rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr rr .. .. .. "
  "rr rr rr .. .. .. .. .. .. .. rr rr .. rr .. .. .. .. rr "
  "rr rr .. .. .. .. .. .. .. .. .. .. .. .. .. rr rr .. rr "
  "rr rr .. .. .. .. .. .. .. .. .. .. .. .. rr rr rr rr rr "
  "rr rr rr .. .. .. .. .. rr rr rr .. .. rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 )

(mk-dungeon-room
 'p_lost_halls_5 "Lost Halls Feast Room"
 (list
  "rr rr rr rr rr rr rr .. .. .. .. .. rr rr rr rr rr rr rr "
  "rr .. .. .. .. rr rr .. .. .. .. .. rr rr rr rr rr rr rr "
  "rr .. rr bb .. .. rr .. .. .. .. rr rr rr rr rr rr rr rr "
  "rr .. rr rr .. .. rr .. .. .. .. rr rr rr rr rr rr rr rr "
  "rr .. .. .. .. rr rr .. .. .. rr rr rr rr cc cc rr rr rr "
  "rr rr .. rr rr rr rr .. .. .. rr ,, cc cc cc cc cc cc rr "
  "rr .. .. .. rr rr rr .. .. .. .. ,, cc cc cc cc cc cc rr "
  ".. .. bb .. rr rr bb .. .. .. .. pp cc cc 00 00 cc cc rr "
  ".. .. .. .. rr bb && .. .. .. .. ,, cc cc 00 00 cc cc rr "
  ".. .. .. .. rr bb && .. .. .. .. ,, cc cc 00 00 cc cc rr "
  ".. .. bb .. rr bb && .. .. .. .. ,, cc cc 00 00 cc cc rr "
  ".. .. .. .. rr rr bb .. .. .. .. ,, cc cc 00 00 cc cc rr "
  "rr .. .. rr rr rr rr .. .. .. .. pp cc cc 00 00 cc cc rr "
  "rr rr .. .. .. .. rr rr .. .. .. ,, cc cc cc cc cc cc rr "
  "rr rr .. rr rr .. rr rr rr .. rr ,, cc cc cc cc cc cc rr "
  "rr rr .. rr bb .. rr rr rr rr rr rr rr bb cc cc rr rr rr "
  "rr rr .. .. .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr .. .. rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 )

(mk-dungeon-room
 'p_lost_halls_6 "Lost Halls End"
 (list
  "rr rr rr rr rr rr rr rr .. .. tt bb bb bb bb bb bb rr rr "
  "rr rr rr rr rr rr rr tt tt tt .. bb .. .. .. bb tt tt rr "
  "rr rr rr rr rr rr rr tt tt tt .. .. tt bb .. .. tt tt rr "
  "rr rr rr rr rr rr rr rr tt .. bb .. .. .. .. tt tt rr rr "
  "rr rr rr rr rr rr rr rr rr tt tt .. .. bb tt tt rr rr rr "
  "rr rr bb rr rr rr rr rr rr rr rr tt .. .. rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr .. bb .. rr rr rr rr rr "
  "rr rr rr rr bb rr rr rr rr rr rr .. .. .. rr rr rr rr rr "
  "rr rr rr rr rr bb rr rr ,R rr .. .. .. .. rr rr rr rr rr "
  "rr rr rr rr rr rr rr rr bb tt .. bb .. .. %% bb rr rr rr "
  "rr rr rr rr rr rr rr rr .. .. .. .. bb .. %% %% bb rr rr "
  "rr rr rr rr rr rr rr bb .. .. bb .. .. %% %% tt %% ,T rr "
  "rr rr ,, xx xx rr rr bb .. bb tt tt .. __ __ __ __ rr rr "
  "rr ,, ,, ,, xx rr ,A %% .. .. .. tt .. .. __ __ __ __ rr "
  "rr ,, ,, ,, xx rr rr rr bb .. bb .. .. .. __ .. .. __ rr "
  "bb ,, ,, ,, xx rr rr rr rr tt %% __ __ bb __ __ __ __ rr "
  "xx bb xx xx xx rr rr rr ,Q %% __ __ __ __ __ __ bb rr rr "
  "rr rr rr rr rr rr rr rr rr rr bb __ __ __ __ bb ,P rr rr "
  "rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
  )
 )

(mk-dungeon-level 
 (list p_lost_halls_1 p_lost_halls_2 p_lost_halls_3)
 (list p_lost_halls_4 p_lost_halls_5 p_lost_halls_6)
 )
