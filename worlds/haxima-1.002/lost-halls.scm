;;----------------------------------------------------------------------------
;; Troll Cave
;;
;; Big underground complex; created by some civilized race, now a ruin
;; inhabited by trolls and other monsters.
;;----------------------------------------------------------------------------

(kern-load "warritrix.scm")

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
		"rr rr rr .. .. .. .. .. .. .. .. %3 %d rr rr xx .. ,, ,, "
		"rr rr rr .. .. .. .. .. .. %b %% %% rr rr xx rr rr ,, ,, "
		"rr rr rr .. .. .. .. .. rr rr ~1 ~~ rr xx rr ,, ,, ,, ,, "
		"rr rr rr rr .. .. rr rr rr rr -- -- xx rr ,, ,, ,, ,, rr "
		"rr rr rr rr .. .. rr rr rr rr rr rr xx ,, ,, ,, ,, rr rr "
		"rr rr rr .. .. .. rr rr rr rr rr xx rr ,, ,, ,, ,, rr rr "
		"rr rr rr .. .. .. .. .. .. .. rr xx rr ,, ,, ,, ,, rr rr "
		"rr rr rr .. .. .. .. .. .. .. rr xx rr rr rr && rr rr rr "
		"rr rr rr rr .. .. rr rr .. .. .. rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr .. .. .. ,, rr ,, ,, ,, rr rr rr "
	)
 (put (mk-corpse-with-loot) 9 10)
 (put (spawn-pt 'giant-spider) 9 18)
 (put (spawn-pt 'troll) 7 8)
 (put (spawn-pt 'cave-goblin-slinger) 18 8)
 (put (spawn-pt 'green-slime) 2 2)
 (put (apply mk-ladder-up lost-halls-loc) 2 2)
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
		"rr rr tb td ,, ,, rr rr rr rr ,, ,, rr rr xx rr .. .. rr "
		"rr tf t# .. .. ,, rr rr rr rr ,, ,, ,, rr xx rr rr .. rr "
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
		"rr rr rr rr rr rr rr t3 tt t5 .. bb .. .. .. bb t3 t5 rr "
		"rr rr rr rr rr rr rr ta tt tc .. .. tf bb .. tC tt tc rr "
		"rr rr rr rr rr rr rr rr te tB bb .. .. .. tC t3 tc rr rr "
		"rr rr rr rr rr rr rr rr rr tb td tA .. bb tb tc rr rr rr "
		"rr rr bb rr rr rr rr rr rr rr rr tf .. .. rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr .. bb .. rr rr rr rr rr "
		"rr rr rr rr bb rr rr rr rr rr rr .. .. .. rr rr rr rr rr "
		"rr rr rr rr rr bb rr rr ,R rr .. .. .. .. rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr bb tf .. bb .. .. %7 bb rr rr rr "
		"rr rr rr rr rr rr rr rr .. .. .. .. bb .. %% %d bb rr rr "
		"rr rr rr rr rr rr rr bb .. .. bb .. .. %3 %% tf %3 ,T rr "
		"rr rr ,, xx xx rr rr bb .. bb tb t5 .. _b _1 _1 __ rr rr "
		"rr ,, ,, ,, xx rr ,A %d .. .. t% te .. ~% __ __ __ _5 rr "
		"rr ,, ,, ,, xx rr rr rr bb .. bb .. .. .. __ ~B ~D _4 rr "
		"bb ,, ,, ,, xx rr rr rr rr tf %3 __ _1 bb __ __ __ _c rr "
		"xx bb xx xx xx rr rr rr ,Q %% _3 __ __ __ __ __ bb rr rr "
		"rr rr rr rr rr rr rr rr rr rr bb _8 _8 _8 _8 bb ,P rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
	)
 (put (spawn-pt 'yellow-slime) 16 11)

 (put (mk-corpse2
       '(
        (1 't_rune_l)
        (1 't_armor_chain_4)
        (1 't_chain_coif_4)
        (1 't_sword_4)
        (1 't_shield_4)
        (1 't_warritrix_orders)
        )) 12 12)
 )

(drop-random-corpses p_lost_halls_6 5)

(mk-dungeon-level 
 (list p_lost_halls_1 p_lost_halls_2 p_lost_halls_3)
 (list p_lost_halls_4 p_lost_halls_5 p_lost_halls_6)
 )
