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
 (put (spawn-pt 'giant-spider) 9 18)
 (put (spawn-pt 'troll) 7 8)
 (put (spawn-pt 'cave-goblin-slinger) 18 8)
 (put (spawn-pt 'green-slime) 2 2)
 (put (mk-ladder-up 'p_shard 82 20) 2 2)
 )

(drop-random-corpses p_lost_halls_1 1)

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
 (put (spawn-pt 'cave-goblin-slinger) 0 9)
 (put (spawn-pt 'gint-warrior) 9 9)
 )

(drop-random-corpses p_lost_halls_2 2)

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
 (put (spawn-pt 'green-slime) 9 18)
 (put (spawn-pt 'gint-warrior) 13 9)
 (put (spawn-pt 'gint-mage) 14 10)
 )

(drop-random-corpses p_lost_halls_3 2)

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
 (put (spawn-pt 'giant-spider) 5 4)
 (put (spawn-pt 'giant-spider) 5 13)
 (put (spawn-pt 'troll) 9 0)
 )

(drop-random-corpses p_lost_halls_4 5)

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
 (put (spawn-pt 'giant-spider) 0 9)
 (put (spawn-pt 'gint-warrior) 13 8)
 (put (spawn-pt 'gint-warrior) 16 10)
 (put (spawn-pt 'gint-mage) 14 13)
 )

(mk-dungeon-room
 'p_lost_halls_6 "Lost Halls End"
 (list
  "rr rr rr rr rr rr rr rr .. .. .. bb bb bb bb bb bb rr rr "
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
 (put (spawn-pt 'yellow-slime) 16 11)

 (put (mk-corpse2
       (mk-contents
        (add-content 't_rune_l 1)
        (add-content 't_armor_chain_4 1)
        (add-content 't_chain_coif_4 1)
        (add-content 't_sword_4 1)
        (add-content 't_shield_4 1)
        (add-content 't_warritrix_orders 1)
        )) 12 12)
 )

(drop-random-corpses p_lost_halls_6 5)

(mk-dungeon-level 
 (list p_lost_halls_1 p_lost_halls_2 p_lost_halls_3)
 (list p_lost_halls_4 p_lost_halls_5 p_lost_halls_6)
 )
