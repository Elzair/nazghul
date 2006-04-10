(mk-dungeon-room
 'p_fire_sea "Fire Sea"
	(list
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr !_ !_ !! {A {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ {{ rr rr rr "
		"rr !_ !_ !_ !5 {A {{ {{ {3 {9 {9 {9 {9 {5 {{ {C ~! rr rr "
		"rr !! !_ !! !! !5 {{ {3 {c {{ {{ {{ {{ {6 {{ ~! !_ ~! rr "
		"rr {% !e {# {{ !! {{ {6 {{ {C !3 !5 {{ {6 {{ {% ~! {# rr "
		"rr {{ {{ {3 {9 == {9 {c {C !3 !_ !! {{ {6 {{ !7 {{ {{ rr "
		"rr {{ {3 {c {{ !! {{ {{ !_ !_ !_ !_ {{ {a {9 == {9 {1 rr "
		"rr {{ {6 {{ {{ !_ !_ !_ rr rr rr rr !5 {{ {{ !! {{ {2 rr "
		"rr {{ {6 {{ {{ {% !a !_ rr .. .. rr rr !! !! !c {{ {2 rr "
		"rr {{ {a {9 {9 {5 {{ rr rr .. .. .. rr {{ !! {{ {{ {2 rr "
		"rr {{ {{ {{ {{ {6 {{ rr rr .. bb .. .. {9 == {9 {9 {8 rr "
		"rr {C !3 !! !! =| !! !_ rr .. .. .. rr {{ !! {{ {{ {{ rr "
		"rr !! !_ !c {{ {6 {{ !_ rr rr rr .. rr rr !_ !! !5 {A rr "
		"rr {% !e {# {{ {6 {{ !! !_ !_ rr rr rr rr !_ !_ !_ !! rr "
		"rr rr {{ {{ {{ {6 {{ !a !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ rr "
		"rr rr rr {{ {{ {6 {{ {% !a !! !! !_ !_ !_ !_ !_ !_ !_ rr "
		"rr rr rr bb {{ {a {5 {{ {{ {{ {% !a !! !! !_ !_ !_ !_ rr "
		"rr rr rr rr bb {{ {2 {1 {1 {5 {{ {{ {{ {{ !! !_ !_ !_ rr "
		"rr rr rr rr rr rr rr rr .. .. {5 {{ {{ {3 bb !! !! ~! rr "
	)

 (put (kern-mk-obj t_rune_w 1) 11 12)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 9 8)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 10 8)
 (put (kern-mk-obj t_iron_helm_4 1) 10 8)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 9 9)
 (put (kern-mk-obj t_gem (kern-dice-roll "5d4+1")) 10 9)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 11 9)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 9 10)
 (put (kern-mk-obj t_sword_4 1) 9 10)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 11 10)
 (put (kern-mk-obj t_armor_plate_4 1) 11 11)
 (put (kern-mk-obj t_gem (kern-dice-roll "5d4+1")) 9 11)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 10 11)
 (put (kern-mk-obj t_shield_4 1) 10 11)
 (put (kern-mk-obj t_gem (kern-dice-roll "5d4+1")) 11 11)
 (put (kern-mk-obj t_gold_coins (kern-dice-roll "5d50+5")) 11 12)

 (put (spawn-pt 'dragon) 11 10)
 )

(mk-dungeon-room
 'p_smoldering_cave "Smoldering Cave"
	(list
		"rr rr rr rr rr rr rr rr .. .. .. rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr {8 .. {8 rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr !_ !_ rr {{ {6 {{ rr !_ !_ rr rr rr rr rr "
		"rr rr rr rr !_ !_ !_ !! {{ {6 {{ !! !_ !_ !_ rr rr rr rr "
		"rr rr rr !_ !_ !_ !_ !_ !! =| !! !_ !_ !_ !_ !_ rr rr rr "
		"rr rr rr !_ !_ !_ !_ !c {{ {6 {{ !a !_ !_ !_ !_ rr rr rr "
		"rr rr !_ !_ !_ !_ !c bb {{ {6 {{ bb !a !_ !_ !_ !_ rr rr "
		"rr rr !_ !_ !_ !! bb {{ {{ {6 {{ {{ bb !! !_ !_ !_ rr rr "
		"rr rr !_ !_ !_ !! {{ {{ {3 .. {5 {{ {{ !! !_ !_ !_ rr rr "
		"rr rr !_ !_ !_ !! {{ {{ {2 .. {4 {{ {{ !! !_ !_ !_ rr rr "
		"rr rr !_ !_ !_ !! {{ {{ {a {8 {c {{ {{ !! !_ !_ !_ rr rr "
		"rr rr !_ !_ !_ !! bb {{ {{ {{ {{ {{ bb !! !_ !_ !_ rr rr "
		"rr rr !_ !_ !_ !_ !5 bb {{ {{ {{ bb !3 !_ !_ !_ !_ rr rr "
		"rr rr rr !_ !_ !_ !_ !! !! !! !! !! !_ !_ !_ !_ rr rr rr "
		"rr rr rr !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ rr rr rr "
		"rr rr rr rr !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ !_ rr rr rr rr "
		"rr rr rr rr rr rr !_ !_ !_ !_ !_ !_ !_ rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
		"rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr rr "
	)
 (put (mk-ladder-up 'p_shard 118 46) 9 9)
 (put (spawn-pt 'fire-slime) 9 1)
 (put (spawn-pt 'fire-slime) 7 3)
 (put (spawn-pt 'fire-slime) 10 4)
 )

(mk-dungeon-level 
 (list p_fire_sea)
 (list p_smoldering_cave)
 )
