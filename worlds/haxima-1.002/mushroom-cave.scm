;;----------------------------------------------------------------------------
;; Map
;;----------------------------------------------------------------------------
(mk-dungeon-room
 'p_mushroom_cave "Mushroom Cave"
	(list
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn r8 r8 rn rn r8 r8 r8 r8 r8 rn rn rn r8 rn rn rn rn rn "
		"r4 %3 %d r2 rc tf .. .. .. tf ra rn r4 {{ ra rn rn rn rn "
		"r4 %% r3 rc |X t# .. .. .. t% |t ra r4 tf {A r2 rn rn rn "
		"r4 %% r6 tf %7 bb .. .. .. bb %7 |% r6 t% t7 r2 rn rn rn "
		"r4 %e r6 %b %% %% %5 .. .. %b %% te r6 .. te r2 rn rn rn "
		"rn r9 rc t7 %% bb %e .. .. bb %% %5 r6 {c {{ r2 rn rn rn "
		"r4 .. |B |X %% %d .. .. .. %b %% %% r6 {{ {{ r2 rn rn rn "
		"r4 .. r7 || %% bb .. %b %5 bb %a %c r6 {{ {{ r2 rn rn rn "
		"r4 t7 r6 te %% .. .. .. %a %d t3 t5 r6 {5 {{ r2 rn rn rn "
		"r4 tt r6 %b %c bb .. .. .. bb |X |C r6 {4 {{ r2 rn rn rn "
		"r4 te ra r5 tf %f .. .. .. tb |X r3 rc .. t7 r2 rn rn rn "
		"r4 t% tf ra r9 r5 .. .. .. r3 r9 rc .. tC tt r2 rn rn rn "
		"r4 .. .. .. && ra r5 .. r3 rc .. .. tC t3 |C r2 rn rn rn "
		"r4 .. .. .. .. .. r2 r1 r4 {{ {2 tC t3 |C r3 rn rn rn rn "
		"rn r5 tf .. .. r3 rn rn rn r5 tb tt |C r3 rn rn rn rn rn "
		"rn rn r1 r1 r1 rn rn rn rn rn r1 r1 r1 rn rn rn rn rn rn "
	)
 (put (spawn-pt 'yellow-slime) 7 7)
 (put (mk-ladder-up 'p_shard 78 74) 7 12)
 (put (kern-mk-obj t_royal_cape 1) 4 15)
 )

(mk-place-music p_mushroom_cave 'ml-dungeon-adventure)
