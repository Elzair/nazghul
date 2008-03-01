;
; I wanted to make the area down here too big and nasty for the paladins to have wiped out.
; So I thought Id just make a more or less infinite pseudorandom dungeon.
; The players dont need to go out there, but they can wander around if they like.
; However, if they do, it is quite large enough for them to get lost and die in.
;
; TODO: occasional 'teleportation' to make "get lost and die" that much easier
;    this latter should occasionally connect a room to a room with a matching
;    edge at a much different location to the expected one.
;

(kern-mk-place 
	'p_lost_garrison
	"Deep Fortress"
	nil          ; sprite
	(kern-mk-map nil 38 38 pal_expanded
		(list
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. ra rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. .. r2 rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r3 rn xx xx xx rn rn rn xx xx xx rn rn rn xx xx rn rn rn xx xx rn rn rn "
		"rn r8 rn xx xx xx w+ xx ,, x! ,, xx w+ xx xx xx xx xx rr x! xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx xx rn rn "
		"rc .. ra xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr ,, ,, ,, ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx rn rn rn "
		".. .. .. rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, .. ,, ,, xx ,, ,, ,, xx ,, ,, xx ,, xx ,, ,, xx xx rn rn "
		".. .. .. w+ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, rr xx xx x! ,, x! xx xx xx xx rn rn "
		".. .. .. xx ,, ,, .. ,, ,, ,, .. bb ,, ,, ,, xx ,, ,, ,, ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx rn rn rn "
		".. .. .. ,, ,, ,, ,, ,, ,, ,, ,, .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, xx ,, xx ,, ,, xx rn rn rn "
		".. .. .. x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, ,, x! ,, ,, ,, xx xx xx xx ,, xx xx xx xx rn rn rn "
		".. .. .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx xx rn rn "
		".. .. .. xx ,, ,, bb ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx xx rn rn "
		".. .. .. w+ ,, ,, ,, ,, ,, ,, ,, ,, .. ,, ,, w+ ,, ,, ,, ,, ,, ,, w+ ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx rn rn "
		".. .. .. xx ,, ,, ,, ,, ,, .. ,, ,, bb ,, ,, xx ,, ,, ,, ,, ,, .. xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx rn rn rn "
		"r1 r5 .. xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, .. .. .. xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx rn rn rn "
		"rn rn xx xx xx xx w+ w+ x! w+ x! w+ w+ xx xx xx xx xx xx x! xx xx xx ,, ,, ,, x! ,, [[ @@ @@ @@ ]] ,, x! xx rn rn "
		"rn rn xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx rn xx xx rn xx xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx rn rn rn "
		"rn rn rn rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, bb r2 rn rn rn rn rn xx ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx rn rn rn "
		"rn rn rn x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! rn rn rn rn rn rn xx ,, ,, ,, xx xx xx xx xx xx xx xx xx xx rn rn "
		"rn rn xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rn rn rn rn rn rn xx ,, ,, ,, xx [[ .M .E .D .I .K ]] xx xx rn rn "
		"rn rn xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx rn rn rn rn rn rn rr ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx rn rn rn "
		"rn rn rn xx xx rr xx rr xx xx xx xx ,, ,, ,, xx xx rn rn xx xx xx rr ,, ,, ,, x! ,, ,, ,, ,, ,, ,, ,, x! rn rn rn "
		"rn rn rn xx ,, ,, ,, [[ @@ @@ ]] xx ,, ,, ,, x! xx xx x! xx xx xx x! ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, bb r2 rn rn "
		"rn rn rn xx 00 ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx rn rn rn "
		"rn rn xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx xx rn rn "
		"rn rn rn xx ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, xx xx rn rn "
		"rn rn xx xx xx xx xx xx x! xx xx xx xx xx xx ,, ,, xx xx xx xx ,, ,, xx xx xx xx xx xx xx xx xx rr xx xx rn rn rn "
		"rn rn xx xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, x! ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, 00 xx rn rn rn "
		"rn rn rn xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, && xx xx rn rn "
		"rn rn rn rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, [[ @@ @@ @@ @@ @@ @@ @@ @@ ]] ,, ,, ,, 00 xx xx rn rn "
		"rn rn rn rr .. ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, && xx rn rn rn "
		"rn rn rn xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, 00 xx rn rn rn "
		"rn rn xx rr ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, rr ,, [[ @@ @@ @@ @@ @@ @@ @@ @@ ]] ,, ,, ,, && xx xx rn rn "
		"rn rn xx bb ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, xx ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, ,, 00 xx xx xx rn "
		"rn rn xx xx xx xx xx xx xx xx xx rr xx xx xx xx xx xx xx xx xx x! xx xx xx xx xx x! xx xx xx xx xx xx xx xx rn rn "
		"rn rn rn xx rn xx xx xx rn rn xx xx xx xx rn rn rn xx xx xx xx xx rn xx rn xx rn xx xx rn xx xx rn rn xx rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "
		"rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn rn "		)	
	)

	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	 (list (put (mk-monman) 0 0) ; objects
		(put (mk-ladder-up 'p_deepness 12 9) 11 11)
		(put (kern-tag 'ednp1 (mk-portcullis)) 8 3)
		(put (kern-tag 'ednp2 (mk-connected-portcullis 'ednp1)) 10 3)
		(put (kern-tag 'edwp1 (mk-portcullis)) 3 8)
		(put (kern-tag 'edwp2 (mk-connected-portcullis 'edwp1)) 3 10)
		(put (kern-tag 'ede1p1 (mk-portcullis)) 15 8)
		(put (kern-tag 'ede1p2 (mk-connected-portcullis 'ede1p1)) 15 10)
		(put (kern-tag 'ede2p1 (mk-portcullis)) 22 8)
		(put (kern-tag 'ede2p2 (mk-connected-portcullis 'ede2p1)) 22 10)
		(put (mk-locked-door) 29 4)
		(put (mk-door) 29 7)
		(put (mk-door) 31 4)
		(put (mk-locked-door) 31 7)
		(put (mk-door) 26 12)
		(put (mk-door) 26 24)
		(put (mk-door) 21 26)
		(put (mk-door) 22 26)
		(put (mk-door) 15 26)
		(put (mk-door) 16 26)
		(put (mk-door) 11 24)
	
		(put (mk-lever 'edwp2) 4 17)
		(put (mk-lever-on 'ednp2) 4 16)
		(put (mk-lever-on 'ede1p2) 14 16)
		(put (mk-lever-on 'ede2p2) 25 4)
	
		(put (mk-bed) 4 25)
		(put (mk-bed) 5 28)
		(put (mk-bed) 8 28)
		(put (mk-bed) 11 28)
		(put (mk-bed) 14 28)
		(put (mk-bed) 17 28)
		(put (mk-bed) 5 30)
		(put (mk-bed) 8 30)
		(put (mk-bed) 11 30)
		(put (mk-bed) 14 30)
		(put (mk-bed) 17 30)
		(put (mk-bed) 5 32)
		(put (mk-bed) 8 32)
		(put (mk-bed) 11 32)
		(put (mk-bed) 14 32)
		(put (mk-bed) 17 32)
		(put (mk-bed) 28 21)
		(put (mk-bed) 32 24)
		(put (mk-bed) 30 24)
		(put (mk-bed) 32 22)
		
		(put (mk-broken-clock s_clock_hand_sw s_clock_hand_se "The clock reads 8:27") 4 22)
		(put (kern-mk-obj t_garrison_log 1) 4 23)
		(put (kern-mk-obj t_food 21) 33 27)
		(put (kern-mk-obj t_food 1) 22 29)
		(put (kern-mk-obj t_food 1) 27 32)
		
		(put (spawn-pt 'troll-m) 6 17)
		(put (spawn-pt 'troll-m) 11 19)
		(put (spawn-pt 'cave-goblin-berserker-m) 29 11)
		(put (spawn-pt 'cave-goblin-slinger-m) 32 13)
		(put (spawn-pt 'cave-goblin-berserker-m) 32 23)
		(put (spawn-pt 'gint-warrior-m) 23 28)
		(put (spawn-pt 'gint-warrior-m) 28 33)
		
		;;guards possessed or dead- captain + 11 troops
		(put (mk-corpse2
		   '(
			(1 t_armor_chain)
			(1 t_sword_2)
			(1 t_scratched_shield)
			)) 7 24)
		
		
		(put (mk-npc 'corrupt-halberdier 4) 18 12)
		(put (mk-npc 'corrupt-halberdier 8) 17 6)
		(put (mk-npc 'corrupt-halberdier 6) 24 12)
		(put (mk-npc 'corrupt-crossbowman 5) 8 17)
		(put (mk-npc 'corrupt-crossbowman 6) 14 5)
		
		(put (mk-npc 'corrupt-halberdier 7) 24 5)
		(put (mk-corpse2
		   '(
			(2 t_heal_potion)
			(1 t_crossbow)
			(13 t_bolt)
			)) 13 6)
		(put (mk-corpse-with-loot) 11 9)
		(put (mk-corpse-with-loot) 17 13)
		(put (mk-corpse-with-loot) 7 29)

		(put (mk-corpse-with-loot) 30 30)
		
		
		;;additional critters
		;;gazers
		(put (mk-npc 'gazer 9) 25 8)
		(put (mk-npc 'gazer 8) 22 31)
		(put (mk-npc 'gazer 7) 7 25)
		(put (mk-npc 'gazer 6) 32 11)
		
		;;gints
		(put (mk-npc 'gint-warrior-m 8) 5 6)
		(put (mk-npc 'gint-warrior-m 7) 12 32)
		(put (mk-npc 'gint-warrior-m 6) 30 27)
		
		;;trolls
		(put (mk-npc 'troll-m 7) 7 19)
		(put (mk-npc 'troll-m 6) 29 17)		
		(put (mk-npc 'troll-m 5) 10 29)
		(put (mk-npc 'troll-m 5) 31 21)
		(put (mk-npc 'troll-m 4) 13 16)
		
		;;goblins
		(put (mk-npc 'cave-goblin-berserker-m 6) 12 31)
		(put (mk-npc 'cave-goblin-berserker-m 5) 9 32)
		(put (mk-npc 'cave-goblin-berserker-m 4) 18 8)
		(put (mk-npc 'cave-goblin-slinger-m 6) 7 20)
		(put (mk-npc 'cave-goblin-slinger-m 5) 12 16)
		(put (mk-npc 'cave-goblin-slinger-m 4) 10 32)
		(put (mk-npc 'cave-goblin-slinger-m 3) 32 16)
		
		;; Rune
		(put (mk-buried 't_rune_p 1) 3 33)
	 )
 
 
	 (list
		'on-entry-to-dungeon-room
		'deeps-room-handle-garrison
		) ;; hooks
		(list ;; edge entrances
			(list east 0 9)
			(list south 9 0)
		)
 )

(mk-place-music p_lost_garrison 'ml-dungeon-adventure)
 


(kern-mk-place 
	'p_deeps_1
	"endless deepness"
	 nil          ; sprite
	(kern-mk-map nil 19 19 pal_expanded
		(list
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn rc .. .. .. .. .. .. .. .. .. ra rn rn rn rn "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"r8 r8 rc .. .. .. .. .. .. .. .. .. .. .. .. .. ra r8 r8 "
		".. .. .. .. .. .. .A .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		"r1 r1 r5 .. .. .. .. .. .. .. .. .. .. .. .. .. r3 r1 r1 "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"rn rn rn rn r5 .. .. .. .. .. .. .. .. .. r3 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		)
	)
	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	nil; objects
	 
	 (list
		;'on-entry-to-dungeon-room
		'deeps-room-handle-deeps
	) ;; hooks
	nil
)

 
 (kern-mk-place 
	'p_deeps_2
	"endless deepness"
	nil          ; sprite
	(kern-mk-map nil 19 19 pal_expanded
		(list
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn rc .. .. .. .. .. .. .. .. .. ra rn rn rn rn "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"r8 r8 rc .. .. .. .. .. .. .. .. .. .. .. .. .. ra r8 r8 "
		".. .. .. .. .. .. .A .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		"r1 r1 r5 .. .. .. .. .. .. .. .. .. .. .. .. .. r3 r1 r1 "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"rn rn rn rn r5 .. .. .. .. .. .. .. .. .. r3 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		)
	)
	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	 (list (put (mk-monman) 0 0) ; objects
	 )
 
	 (list
		;'on-entry-to-dungeon-room
		'deeps-room-handle-deeps
	) ;; hooks
	nil
)
 
   (kern-mk-place 
	'p_deeps_3
	"endless deepness"
	 nil          ; sprite
	(kern-mk-map nil 19 19 pal_expanded
		(list
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn rc .. .. .. .. .. .. .. .. .. ra rn rn rn rn "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"r8 r8 rc .. .. .. .. .. .. .. .. .. .. .. .. .. ra r8 r8 "
		".. .. .. .. .. .. .A .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		"r1 r1 r5 .. .. .. .. .. .. .. .. .. .. .. .. .. r3 r1 r1 "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"rn rn rn rn r5 .. .. .. .. .. .. .. .. .. r3 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		)
	)
	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	 (list (put (mk-monman) 0 0) ; objects
	 )
	 
	 (list
		;'on-entry-to-dungeon-room
		'deeps-room-handle-deeps
	) ;; hooks
 	nil
 )
 
   (kern-mk-place 
	'p_deeps_4
	"endless deepness"
	 nil          ; sprite
	(kern-mk-map nil 19 19 pal_expanded
		(list
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn rc .. .. .. .. .. .. .. .. .. ra rn rn rn rn "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"r8 r8 rc .. .. .. .. .. .. .. .. .. .. .. .. .. ra r8 r8 "
		".. .. .. .. .. .. .A .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		"r1 r1 r5 .. .. .. .. .. .. .. .. .. .. .. .. .. r3 r1 r1 "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"rn rn rn rn r5 .. .. .. .. .. .. .. .. .. r3 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		)
	)
	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	 (list (put (mk-monman) 0 0) ; objects
	 )
	 
	 (list
		;'on-entry-to-dungeon-room
		'deeps-room-handle-deeps
	) ;; hooks
 	nil
 )
 
 (kern-mk-place 
	'p_deeps_5
	"endless deepness"
	  nil          ; sprite
	(kern-mk-map nil 19 19 pal_expanded
		(list
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn rc .. .. .. .. .. .. .. .. .. ra rn rn rn rn "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"r8 r8 rc .. .. .. .. .. .. .. .. .. .. .. .. .. ra r8 r8 "
		".. .. .. .. .. .. .A .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		".. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. .. "
		"r1 r1 r5 .. .. .. .. .. .. .. .. .. .. .. .. .. r3 r1 r1 "
		"rn rn rn .. .. .. .. .. .. .. .. .. .. .. .. .. rn rn rn "
		"rn rn rn rn r5 .. .. .. .. .. .. .. .. .. r3 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		"rn rn rn rn r4 .. .. .. .. .. .. .. .. .. r2 rn rn rn rn "
		)
	)
	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	 (list (put (mk-monman) 0 0) ; objects
	 )
	 
	 (list
		;'on-entry-to-dungeon-room
		'deeps-room-handle-deeps
	) ;; hooks
	nil
 )
 

(mk-place-music p_deeps_1 'ml-dungeon-adventure)
(mk-place-music p_deeps_2 'ml-dungeon-adventure)
(mk-place-music p_deeps_3 'ml-dungeon-adventure)
(mk-place-music p_deeps_4 'ml-dungeon-adventure)
(mk-place-music p_deeps_5 'ml-dungeon-adventure)

 (kern-mk-place 
	'deeps_data
	"null"
	  nil          ; sprite
	(kern-mk-map nil 1 1 pal_expanded
		(list
		  "rr"
		)
	)
	 #f              ; wraps
	 #t              ; underground
	 #f              ; large-scale (wilderness)
	 #f              ; tmp combat place
	 nil ; subplaces
	 nil ; neighbors
 
	nil ;; objects
	nil ;; hooks
	nil
 )

(prmap-linkrooms-2d 'p_deeps_1 'p_deeps_2 'p_deeps_3 'p_deeps_4 'p_deeps_5)
(prmap-mk-roomdata 'p_lost_garrison 0 0 0 (list 'p_deeps_1 'p_deeps_2 'p_deeps_5 'p_deeps_3))

(prmap-set-mapdata p_deeps_1 (prmap-mk-mapdata "endless_deeps" deep-random-type-ns deep-random-type-ew deep-random-type-area 'deep-terrain-edges 'deep-terrain-area 'deep-room-blitstats nil))

(let ((deep-hardlinks (prmap-params-hardlinks (prmap-get-mapdata p_deeps_1))))
	(define (link-rm xloc yloc zloc dir target maptemplate passable )
		(prmap-room-hardlink-set! xloc yloc zloc deep-hardlinks dir target maptemplate passable nil)
		)
		
;;---------------------------------------------------------
;; hardlink setup
;; 
;;			x	y	z	dir		target				template				passable

(link-rm	0	1	0	south	'p_lost_garrison	'm_deeptempl_passage	#f)
(link-rm	1	1	0	south	nil					'm_deeptempl_wall		#f)
(link-rm	-1	0	0	east	'p_lost_garrison	'm_deeptempl_passage	#f)
(link-rm	2	0	0	west	nil					'm_deeptempl_wall		#f)
(link-rm	-1	-1	0	east	nil					'm_deeptempl_wall		#f)
(link-rm	2	-1	0	west	nil					'm_deeptempl_wall		#f)
(link-rm	0	-2	0	north	nil					'm_deeptempl_wall		#f)
(link-rm	1	-2	0	north	nil 				'm_deeptempl_wall		#f)
)

;;flag for checking if cohesion check still needs to be performed
(mutable-list-set (prmap-get-mapdata p_deeps_1) 10 #t)