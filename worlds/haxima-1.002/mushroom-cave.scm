;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------
(mk-dungeon-room
 'p_mushroom_cave "Mushroom Cave"
	(list
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr %3 %d rr rr tf .. .. .. tf rr rr rr {{ rr rr rr rr rr "
		"rr %% rr rr || t# .. .. .. t% || rr rr tf {A rr rr rr rr "
		"rr %% rr tf %7 bb .. .. .. bb %7 |% rr t% t7 rr rr rr rr "
		"rr %e rr %b %% %% %5 .. .. %b %% te rr .. te rr rr rr rr "
		"rr rr rr t7 %% bb %e .. .. bb %% %5 rr {c {{ rr rr rr rr "
		"rr .. |B || %% %d .. .. .. %b %% %% rr {{ {{ rr rr rr rr "
		"rr .. rr || %% bb .. %b %5 bb %a %c rr {{ {{ rr rr rr rr "
		"rr t7 rr te %% .. .. .. %a %d t3 t5 rr {5 {{ rr rr rr rr "
		"rr tt rr %b %c bb .. .. .. bb || |C rr {4 {{ rr rr rr rr "
		"rr te rr rr tf %f .. .. .. tb || rr rr .. t7 rr rr rr rr "
		"rr t% tf rr rr rr .. .. .. rr rr rr .. tC tt rr rr rr rr "
		"rr .. .. .. && rr rr .. rr rr .. .. tC t3 |C rr rr rr rr "
		"rr .. .. .. .. .. rr rr rr {{ {2 tC t3 |C rr rr rr rr rr "
		"rr rr tf .. .. rr rr rr rr rr tb tt |C rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
	)
 (put (spawn-pt 'yellow-slime) 7 7)
 (put (mk-ladder-up 'p_shard 78 74) 7 12)
 (put (kern-mk-obj t_royal_cape 1) 4 15)
 )
